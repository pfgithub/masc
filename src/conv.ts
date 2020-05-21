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

type VNM = {
    [key: string]: {
        type: Type;
        tempname: string; // before returning, replace all instances of tempname with the actual name
    };
};

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
        let va = vnm[expr.var];
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
    let eai = evalExprAllowImmediate(vnm, expr);
    if (eai !== exprNotAvailable) {
        lines.push(`move ${out} ${eai.reg}`);
        return eai.typ;
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

function mipsgen(ast: Ast[]): string[] {
    let ress: string[] = [];
    let varNameMap: VNM = {};
    for (let line of ast) {
        let res: string[] = [];
        if (line.ast === "ilasm") {
            res.push(line.ilasm);
        } else if (line.ast === "clear") {
            // TODO
        } else if (line.ast === "defvar") {
            if (varNameMap[line.name]) {
                throw new Error("var already exists");
            }
            let tempname = gentemp();
            let type = evalType(line.type);
            // right now this is only one way, it would also be useful to tell evalExpr about what type we expect (if we expect one)
            let rest = evalExpr(varNameMap, line.default, tempname, res);
            matchTypes(rest, type); // in the future, a specified type could be optional by : if no type is specified, set the type to rest
            varNameMap[line.name] = { type, tempname };
        } else if (line.ast === "setvar") {
            let eao = evalExprAnyOut(varNameMap, line.name);
            let rt = evalExpr(varNameMap, line.value, eao.reg, res);
            matchTypes(eao.typ, rt);
        } else {
            asun(line);
        }
        let srccode = code
            .substring(line.pos.start.index, line.pos.end.index)
            .split("\n");
        res.forEach((lne, i) => {
            // distribute source code over these lines evenly
            ress.push(res + " " + commentSeparator + " " + srccode[i] || "");
        });
    }
    // determine good registers for all variables

    let fres: string[] = [];
    let tempassignments: { [key: string]: string } = {};
    for (let line of ress) {
        // find unassigned temps (search down for more copies, if it is cleared inbetween uses, try again)
        // assign
        // replace
        fres.push(line);
    }

    return fres;
}

let res = mipsgen(baseast).join("\n");
fs.writeFileSync(__dirname + "/code.mips", res, "utf-8");
console.log(res);
