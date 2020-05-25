import * as fs from "fs";
import { parse, Ast, AstType, AstExpr } from "./build";

const inputCode = fs
    .readFileSync("src/helloworld.masc", "utf-8")
    .split("\t")
    .join("    ");
const baseast = parse(inputCode) as Ast[];

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
let usedLoopNames: { [key: string]: number } = {};
function genlabel(name: string): string {
    if (usedLoopNames[name]) return name + "_" + ++usedLoopNames[name];
    usedLoopNames[name]++;
    return name + "_0";
}

type Type = "u32" | "i32" | "any" | "void";

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
    out = out.replace("%%:", "%%:out:");
    // run an expr and set resregister to the expr result;
    let simpleRegister = evalExprAnyOut(vnm, expr);
    if (simpleRegister !== exprNotAvailable) {
        if (simpleRegister.reg === out) return simpleRegister.typ;
        lines.push(`move ${out} ${simpleRegister.reg}`);
        return simpleRegister.typ;
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
    } else if (expr.expr === "call") {
        let ce = vnm.getfn(expr.name);
        if (!ce) throw new Error("unknown fn " + expr.name);
        return ce(expr.args, lines);
    }
    throw new Error("Not implemented expr: " + expr.expr);
}

type VarInfo = {
    type: Type;
    tempname: string;
};
type LoopInfo = {
    start: string;
    end: string;
};
type FnHndlr = (args: AstExpr[], res: string[]) => Type;
type VNM = {
    getfn: (key: string) => FnHndlr | undefined;
    setfn(key: string, hndlr: FnHndlr): void;
    get: (key: string) => VarInfo | undefined;
    set: (key: string, value: VarInfo) => void;
    getLoop: () => LoopInfo | undefined;
    setLoop(nv: LoopInfo | undefined): void;
};
function makeVariableNameMap(parent?: VNM): VNM {
    let map = new Map<string, VarInfo>();
    let fns = new Map<string, (args: AstExpr[], res: string[]) => Type>();
    let latestLoop: LoopInfo | undefined = undefined;
    return {
        getfn(name) {
            return fns.get(name) || (parent ? parent.getfn(name) : undefined);
        },
        setfn(name, value) {
            // should there be a no shadowing rule?
            if (fns.has(name)) throw new Error("fn already defined: " + name);
            fns.set(name, value);
        },
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
        getLoop() {
            return latestLoop || (parent ? parent.getLoop() : undefined);
        },
        setLoop(nv) {
            latestLoop = nv;
        },
    };
}

function mipsgen(ast: Ast[], parentVNM?: VNM): string[] {
    // find all unordered declarations (eg functions)
    // init their types and stuff

    let finalResultCode: string[] = [];
    let vnm: VNM = makeVariableNameMap(parentVNM);

    // note, this is only for predeclaring. any actual code insertion
    // should happen below so it stays in-order
    for (const line of ast) {
        if (line.ast === "fn") {
            if (!line.inline) throw new Error("inline only atm");
            let type = evalType(line.type);
            let expctArgs = line.args.map(arg => ({
                typ: evalType(arg.type),
                name: arg.name,
            }));
            let returnMark = genlabel(line.name + "_return");
            vnm.setfn(line.name, (args, reslines) => {
                // might have to define a label and say where return
                // should go here so if the inline fn has a return
                // instr, it jumps to the right place instead of
                // jring nowhere. also make sure to provide the
                // type so the return instr can typecheck the thing
                // it is passed.
                let nvnm = makeVariableNameMap(vnm);
                if (args.length !== expctArgs.length)
                    throw new Error("wrong arg count");
                expctArgs.forEach((expctArg, i) => {
                    let arg = args[i];
                    let resvar = gentemp();
                    let typ = evalExpr(vnm, arg, resvar, reslines);
                    matchTypes(typ, expctArg.typ);
                    nvnm.set(expctArg.name, {
                        type: expctArg.typ,
                        tempname: resvar,
                    });
                });
                reslines.push(
                    ...mipsgen(line.body, nvnm).map(
                        l => l.split(commentSeparator)[0],
                    ),
                );
                // reslines.push(returnMark + ":"); // todo remove unused labels
                return type;
            });
        }
    }

    for (let line of ast) {
        let code: string[] = [];
        if (line.ast === "ilasm") {
            code.push(line.ilasm + commentSeparator);
        } else if (line.ast === "clear") {
            code.push(
                "%%{{MARK_CLEAR:" +
                    line.registers
                        .flatMap(r => regExpansions[r] || [r])
                        .join(",") +
                    "}}%%",
            );
        } else if (line.ast === "defvar") {
            if (vnm.get(line.name)) {
                throw new Error("var already exists");
            }
            let tempname = gentemp();
            let defType = evalType(line.type);
            // right now this is only one way, it would also be useful to tell evalExpr about what type we expect (if we expect one)
            let exprResultType = evalExpr(vnm, line.default, tempname, code);
            matchTypes(defType, exprResultType); // in the future, a specified type could be optional by : if no type is specified, set the type to rest
            vnm.set(line.name, { type: defType, tempname });
        } else if (line.ast === "setvar") {
            let eao = evalExprAnyOut(vnm, line.name);
            let rt = evalExpr(vnm, line.value, eao.reg, code);
            matchTypes(eao.typ, rt);
        } else if (line.ast === "if") {
            let left = evalExprAnyOut(vnm, line.condleft, code);
            let right = evalExprAllowImmediate(vnm, line.condright, code);
            let conditionType = matchTypes(left.typ, right.typ);
            let u: string;
            if (conditionType === "u32") u = "u";
            else if (conditionType === "i32") u = "";
            else throw new Error("unsupported if type " + conditionType);
            // this would be much more fun to code in zig
            // I didn't want to because it would require setting up a
            //    parser though and I've already done that twice and
            //    am working on a third

            let lbl = genlabel("if_end");
            let requiresCode = true;

            let rescode = mipsgen(line.code, vnm); // TODO pass in variables
            if (rescode.length === 1 && rescode[0].trim().startsWith("j ")) {
                let jumpinstr = rescode[0]
                    .trim()
                    .split(" ")
                    .slice(1)
                    .join(" ")
                    .split(commentSeparator)[0];
                lbl = jumpinstr;
                requiresCode = false;
            }

            if (line.condition === "==") {
                if (right.reg === "0") code.push(`bnez ${left.reg}, ${lbl}`);
                else code.push(`bne ${left.reg} ${right.reg}, ${lbl}`);
            } else if (line.condition == "!=") {
                if (right.reg === "0") code.push(`beqz ${left.reg}, ${lbl}`);
                else code.push(`beq ${left.reg} ${right.reg}, ${lbl}`);
            } else if (line.condition == ">=") {
                code.push(`blt${u} ${left.reg} ${right.reg}, ${lbl}`);
            } else if (line.condition == ">") {
                code.push(`ble${u} ${left.reg} ${right.reg}, ${lbl}`);
            } else if (line.condition == "<") {
                code.push(`bge${u} ${left.reg} ${right.reg}, ${lbl}`);
            } else if (line.condition == "<=") {
                code.push(`bgt${u} ${left.reg} ${right.reg}, ${lbl}`);
            } else {
                asun(line.condition);
            }
            if (requiresCode) {
                code.push(...rescode.map(l => "    " + l));
                code.push(lbl + ":");
            }
        } else if (line.ast === "loop") {
            let startLabel = genlabel("loop_continue");
            let endLabel = genlabel("loop_end");

            let vctx = makeVariableNameMap(vnm);
            vctx.setLoop({ start: startLabel, end: endLabel });
            let rescode = mipsgen(line.code, vctx);

            code.push(startLabel + ":");
            code.push("%%{{controlflow_mark::" + startLabel + "}}%%");
            code.push(...rescode.map(l => "    " + l));
            code.push("%%{{controlflow_goto::" + startLabel + "}}%%");
            code.push(endLabel + ":" + commentSeparator);
        } else if (line.ast === "continue") {
            let lp = vnm.getLoop();
            if (!lp) throw new Error("continue not in loop");
            code.push("j " + lp.start);
        } else if (line.ast === "break") {
            let lp = vnm.getLoop();
            if (!lp) throw new Error("break not in loop");
            code.push("j " + lp.end);
        } else if (line.ast === "fn") {
            if (!line.inline)
                throw new Error(
                    "non inline fns are not supported to insert code yet",
                );
        } else if (line.ast === "expr") {
            let res = evalExprAnyOut(vnm, line.expr, code);
            if (res.typ !== "void") throw new Error("unused value " + res.typ);
        } else {
            asun(line);
        }
        let spaceCount = 0;
        for (let i = line.pos.start.index - 1; true; i--) {
            if (i < 0 || inputCode[i] === "\n") break;
            spaceCount++;
        }
        let srccode = (
            " ".repeat(spaceCount) +
            inputCode.substring(line.pos.start.index, line.pos.end.index)
        ).split("\n");
        code.forEach((lne, i) => {
            // distribute source code over these lines evenly
            if (lne.includes(commentSeparator)) finalResultCode.push(lne);
            else {
                let codeText = srccode[i] || "";
                finalResultCode.push(lne + commentSeparator + codeText);
            }
        });
        // if (code.length > 0)
        //     for (let i = code.length; i < srccode.length; i++) {
        //         finalResultCode.push("" + commentSeparator + srccode[i]);
        //     }
    }
    return finalResultCode;
}

function finalize(rawIR: string[]): string {
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
    let controlFlowMarks: { [key: string]: number | undefined } = {};
    rawIR.forEach((line, i) => {
        let cfMarkMatch = /%%{{controlflow_mark::(.+?)}}%%/.exec(line);
        if (cfMarkMatch) {
            controlFlowMarks[cfMarkMatch[1]] = i;
        }
    });
    let solveVariableInternal = (
        variableID: string,
        startIndex: number,
        endIndexExclusive: number,
        unavailableRegisters: Set<string>,
        visitedMarks: { [key: string]: true },
    ) => {
        let unavRegisIfReferenced = new Set<string>();
        for (let j = startIndex; j < endIndexExclusive; j++) {
            let line = rawIR[j];
            // if line contains drop, add all listed registers to unavailable
            let clearMarkMatch = /%%{{MARK_CLEAR:(.+?)}}%%/.exec(line);
            if (clearMarkMatch) {
                let clrs = clearMarkMatch[1].split(",");
                clrs.forEach(clr => unavRegisIfReferenced.add(clr));
                continue;
            }
            let cfRevisitMatch = /%%{{controlflow_goto::(.+?)}}%%/.exec(line);
            if (cfRevisitMatch) {
                if (visitedMarks[cfRevisitMatch[1]]) continue;
                visitedMarks[cfRevisitMatch[1]] = true;
                let revisitStart = controlFlowMarks[cfRevisitMatch[1]]!;
                solveVariableInternal(
                    variableID,
                    revisitStart,
                    j,
                    unavailableRegisters,
                    visitedMarks,
                );
                continue;
            }
            // if line contains this variable, move updated to unavailable
            if (line.includes("%%:variable:" + variableID + ":%%")) {
                for (let uur of unavRegisIfReferenced) {
                    unavailableRegisters.add(uur);
                }
                unavRegisIfReferenced.clear(); // unnecessary but why not
                continue;
            }
            if (line.includes("%%:out:variable:" + variableID + ":%%")) {
                unavRegisIfReferenced.clear();
                continue;
            }
            // if line contains other register, add to updatedUnavailable
            line = line.replace(
                /%%:((?:out\:)?)variable:(.+?):%%/g,
                (deflt, outmby, letr) => {
                    if (registerNameMap[letr])
                        return (
                            "%%:" +
                            outmby +
                            "register:" +
                            registerNameMap[letr] +
                            ":%%"
                        );
                    return deflt;
                },
            );
            let regs = [...line.matchAll(/%%:register:(..):%%/g)].map(
                q => q[1],
            );
            let outRegs = [...line.matchAll(/%%:out:register:(..):%%/g)].map(
                q => q[1],
            );
            regs.map(reg => unavailableRegisters.add(reg));
            outRegs.map(reg => unavRegisIfReferenced.add(reg));
        }
    };
    let solveVariable = (variableID: string, startIndex: number) => {
        let unavailableRegisters = new Set<string>([]);
        let visitedMarks: { [key: string]: true } = {};
        solveVariableInternal(
            variableID,
            startIndex,
            rawIR.length,
            unavailableRegisters,
            visitedMarks,
        );
        return unavailableRegisters;
    };
    let registersOnlyIR: string[] = [];
    rawIR.forEach((line, i) => {
        registersOnlyIR.push(
            line.replace(/%%:((?:out\:)?)variable:(.+?):%%/g, (_, om, letr) => {
                if (registerNameMap[letr])
                    return "%%:register:" + registerNameMap[letr] + ":%%";
                let unavailable = solveVariable(letr, i);
                let reg = userRegisters.find(ussr => !unavailable.has(ussr));
                if (!reg) throw new Error("Out of registers!");
                registerNameMap[letr] = reg;
                return "%%:" + om + "register:" + reg + ":%%";
            }),
        );
    });

    let txt = registersOnlyIR
        .map(line =>
            line.replace(/%%:(?:out\:)?register:(..):%%/g, (_, q) => "$" + q),
        )
        .filter(l => !l.trim().startsWith("%%{{"));

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
