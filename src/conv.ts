import * as fs from "fs";
import { parse, Ast, AstType, AstExpr } from "./build";

const code = fs.readFileSync("src/helloworld.masc", "utf-8");
const baseast = parse(code) as Ast[];

let asun = (v: never): never => {
    console.log(v);
    throw new Error("unexpected enumthing");
};

let gtid = 0;
function gentemp(): string {
    return "%%:variable:" + gtid++ + ":%%";
}
function genreg(regnme: string): string {
    return "%%:register:" + regnme + ":%%";
}
function genlabel(): string {
    return "__renameme_" + gtid++;
}

type Type = "u32" | "i32" | "any";

// because prettier doesn't know how to format code sensibly:
// (zig fmt has no problem with this)
// prettier-ignore
let regExpansions: { [key: string]: string[] } = {
    call: [
        "v0", "v1", "a0", "a1", "a2", "a3", "t0",
        "t1", "t2", "t3", "t4", "t5", "t6", "t7",
    ]
};

// use from left to right, preferring left when available
// prettier-ignore
let userRegisters: string[] = [
    "t0", "t1", "t2", "t3", "t4","t5", "t6", "t7",
    "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7",
    "a0", "a1", "a2", "a3",
    "v0", "v1"
];

function evalType(tast: AstType): Type {
    return tast.kind;
}

/*
ok what if

x = 0;     // given register 0
clear $t0; // flags x that t0 will not be allowed if used again
y = 0;     // given register 1
x += 1;    // now actually sets x so t0 is not allowed

// here, y should get t0 and x should get t1
// the register numbers are useless. what is their point?

*/

let exprNotAvailable = ("%%__EXPR__NOT__AVAILABLE%%" as any) as ExprRetV;
let commentSeparator = "%%__COMMENT_SEP__%%";

type ExprRetV = { typ: Type; reg: string };

function evalExprAllowImmediate(
    vnm: VNM,
    expr: AstExpr,
    lines?: string[],
): ExprRetV {
    if (expr.expr === "immediate") {
        return { reg: expr.value, typ: "any" };
    } else {
        return evalExprAnyOut(vnm, expr, lines);
    }
}

function evalExprAnyOut(vnm: VNM, expr: AstExpr, lines?: string[]): ExprRetV {
    if (expr.expr === "register") {
        return { reg: genreg(expr.register), typ: "any" };
    } else if (expr.expr === "variable") {
        let va = vnm.get(expr.var);
        if (!va) throw new Error("variable not found " + expr.var);
        return { reg: va.tempname, typ: va.type };
    } else if (lines) {
        let out = gentemp();
        return { typ: evalExpr(vnm, expr, out, lines), reg: out };
        // TODO: now mark all the intermediate values as unused
        // or do this in a second step later
        // probably in a second step later
    } else {
        return exprNotAvailable;
    }
}

function matchTypes(ta: Type, tb: Type): Type {
    if (ta !== "any") {
        if (tb === "any") return ta;
        if (tb === ta) return ta;
        throw new Error("Incompatible Types: " + ta + ", " + tb);
    }
    if (tb !== "any") {
        if (ta === "any") return tb;
        if (tb === ta) return ta;
        throw new Error("Incompatible Types: " + ta + ", " + tb);
    }
    return "any";
}

// if an expected return value arg is passed, it might be useful
function evalExpr(vnm: VNM, expr: AstExpr, out: string, lines: string[]): Type {
    // run an expr and set resregister to the expr result;
    let eai = evalExprAnyOut(vnm, expr);
    if (eai !== exprNotAvailable) {
        lines.push(`move ${out} ${eai.reg}`);
        return eai.typ;
    } else if (expr.expr === "immediate") {
        lines.push(`li ${out} ${expr.value}`);
        return "any";
    } else if (expr.expr === "add") {
        let a = evalExprAnyOut(vnm, expr.left, lines);
        let b = evalExprAllowImmediate(vnm, expr.right, lines);

        let resType = matchTypes(a.typ, b.typ);
        if (resType === "u32") lines.push(`addu ${out}, ${a.reg} ${b.reg}`);
        else if (resType === "i32") lines.push(`add ${out}, ${a.reg} ${b.reg}`);
        else throw new Error("Add does not support type " + resType);

        return resType;
    } else if (expr.expr === "undefined") {
        return "any";
    }
    throw new Error("Not implemented expr: " + expr.expr);
}

type VarInfo = {
    type: Type;
    tempname: string;
};
type VNM = {
    get: (key: string) => VarInfo | undefined;
    set: (key: string, value: VarInfo) => void;
};
function mkVNM(parent?: VNM): VNM {
    let map = new Map<string, VarInfo>();
    return {
        get(key) {
            let res = map.get(key);
            if (!res && parent) return parent.get(key);
            return res;
        },
        set(key, value) {
            if (map.get(key))
                throw new Error("variable already defined: " + key);
            map.set(key, value);
        },
    };
}

function mipsgen(ast: Ast[], parentVNM?: VNM): string[] {
    let ress: string[] = [];
    let vnm: VNM = mkVNM(parentVNM);
    for (let line of ast) {
        let res: string[] = [];
        let comment = true;
        if (line.ast === "ilasm") {
            comment = false;
            res.push(line.ilasm);
        } else if (line.ast === "clear") {
            res.push(
                "%%:MARK_CLEAR:" +
                    line.registers
                        .flatMap(r => regExpansions[r] || [r])
                        .join(",") +
                    ":%%",
            );
        } else if (line.ast === "defvar") {
            if (vnm.get(line.name)) {
                throw new Error("var already exists");
            }
            let tempname = gentemp();
            let type = evalType(line.type);
            // right now this is only one way, it would also be useful to tell evalExpr about what type we expect (if we expect one)
            let rest = evalExpr(vnm, line.default, tempname, res);
            matchTypes(rest, type); // in the future, a specified type could be optional by : if no type is specified, set the type to rest
            vnm.set(line.name, { type, tempname });
        } else if (line.ast === "setvar") {
            let eao = evalExprAnyOut(vnm, line.name);
            let rt = evalExpr(vnm, line.value, eao.reg, res);
            matchTypes(eao.typ, rt);
        } else if (line.ast === "if") {
            let left = evalExprAnyOut(vnm, line.condleft, res);
            let right = evalExprAllowImmediate(vnm, line.condright, res);
            let condt = matchTypes(left.typ, right.typ);
            let u: string;
            if (condt === "u32") u = "u";
            else if (condt === "i32") u = "";
            else throw new Error("unsupported if type " + condt);
            // this would be much more fun to code in zig
            // I didn't want to because it would require setting up a
            //    parser though and I've already done that twice and
            //    am working on a third
            let lbl = genlabel();
            if (line.condition === "==") {
                if (right.reg === "0") res.push(`bnez ${left.reg}, ${lbl}`);
                else res.push(`bne ${left.reg} ${right.reg}, ${lbl}`);
            } else if (line.condition == "!=") {
                if (right.reg === "0") res.push(`beqz ${left.reg}, ${lbl}`);
                else res.push(`beq ${left.reg} ${right.reg}, ${lbl}`);
            } else if (line.condition == ">=") {
                res.push(`blt${u} ${left.reg} ${right.reg}, ${lbl}`);
            } else if (line.condition == ">") {
                res.push(`ble${u} ${left.reg} ${right.reg}, ${lbl}`);
            } else if (line.condition == "<") {
                res.push(`bge${u} ${left.reg} ${right.reg}, ${lbl}`);
            } else if (line.condition == "<=") {
                res.push(`bgt${u} ${left.reg} ${right.reg}, ${lbl}`);
            } else {
                asun(line.condition);
            }
            let rescode: string[] = mipsgen(line.code, vnm); // TODO pass in variables
            res.push(...rescode.map(l => "    " + l));
            res.push("# todo code");
            res.push(lbl + ":");
        } else {
            asun(line);
        }
        let srccode = code
            .substring(line.pos.start.index, line.pos.end.index)
            .split("\n");
        res.forEach((lne, i) => {
            // distribute source code over these lines evenly
            if (comment && !lne.includes(commentSeparator))
                ress.push(lne + commentSeparator + srccode[i] || "");
            else ress.push(lne);
        });
    }
    return ress;
}

function finalize(inraw: string[]): string {
    // I want this to have a concept of control flow
    // when it goes from top to bottom deciding variables,
    // at an if when it reaches the } it jumps to after the else
    // (eg)

    // wait no this is unnecessary
    // if(a)
    //   let b
    // else
    //   let c
    // end
    // will already give the same registers to b and c because b doesn't exist further on in the file
    // the difficult part is going to be finding lifetimes for loop things

    let registerNameMap: { [key: string]: string } = {};
    let solveVariable = (varbl: string, i: number) => {
        let unavailableRegisters = new Set<string>([]);
        let updatedUnavRegi = new Set<string>();
        for (let j = i; j < inraw.length; j++) {
            let lne = inraw[j];
            // if line contains drop, add all listed registers to unavailable
            let mkclr = /%%:MARK_CLEAR:(.+?):%%/.exec(lne);
            if (mkclr) {
                let clrs = mkclr[1].split(",");
                clrs.forEach(clr => updatedUnavRegi.add(clr));
                continue;
            }
            lne = lne.replace(/%%:variable:(.+?):%%/g, (deflt, letr) => {
                if (registerNameMap[letr])
                    return "%%:register:" + registerNameMap[letr] + ":%%";
                return deflt;
            });
            let regs = [...lne.matchAll(/%%:register:(..):%%/g)].map(q => q[1]);
            // if line contains other register, add to updatedUnavailable
            regs.map(reg => updatedUnavRegi.add(reg));
            // if line contains this variable, move updated to unavailable
            if (lne.includes("%%:variable:" + varbl + ":%%")) {
                for (let uur of updatedUnavRegi) {
                    unavailableRegisters.add(uur);
                }
                updatedUnavRegi.clear(); // unnecessary but why not
            }
        }
        return unavailableRegisters;
    };
    let fres: string[] = [];
    inraw.forEach((line, i) => {
        fres.push(
            line.replace(/%%:variable:(.+?):%%/g, (_, letr) => {
                if (registerNameMap[letr])
                    return "%%:register:" + registerNameMap[letr] + ":%%";
                let unavailable = solveVariable(letr, i);
                let reg = userRegisters.find(ussr => !unavailable.has(ussr));
                if (!reg) throw new Error("Out of registers!");
                registerNameMap[letr] = reg;
                return "%%:register:" + reg + ":%%";
            }),
        );
    });

    let txt = fres
        .map(line => line.replace(/%%:register:(..):%%/g, (_, q) => "$" + q))
        .filter(l => !l.trim().startsWith("%%:MARK_CLEAR"));

    //

    //

    let lsplits = txt.map(l => l.split(commentSeparator));
    let maxLineLen = 12;
    for (let [code, comment] of lsplits) {
        comment = comment || "";
        let tsc = /^ */.exec(comment)![0].length;
        let rl = code.length - tsc + 1;
        if (rl > maxLineLen) maxLineLen = rl;
    }
    let resLines: string[] = [];
    for (let [code, comment] of lsplits) {
        comment = comment || "";
        resLines.push(code.padEnd(maxLineLen, " ") + "# " + comment);
    }
    return resLines.join("\n");
}

let mair = mipsgen(baseast);
fs.writeFileSync(__dirname + "/code.mair", mair.join("\n"), "utf-8");
let res = finalize(mair);
fs.writeFileSync(__dirname + "/code.mips", res, "utf-8");
console.log(res);
