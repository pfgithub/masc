import { parse, Ast, AstType, AstExpr, FnAst, Pos } from "./build";

let enableEndLabel = false;
let todo = () => {
    let lyncol = new Error()
        .stack!.split("\n")[2]
        .replace(/^.+:(\d+?):(\d+?)$/, ":$1:$2");
    return "!TODO" + lyncol + "!";
};

let nvercmnt = (): BlockComment => ({
    msg:
        "You should never see this !!!+!+!+!+!+!)_++#!+_#*(!+*!#(+)#!(*!#*()!#*#!*)!#*+)_!#()_!#+_()#!_()+",
});

let poserr = (pos: Pos, msg: string) => {
    let err = new Error(
        pos.start.line +
            1 +
            ":" +
            (pos.start.col + 1) +
            "-" +
            (pos.end.line + 1) +
            ":" +
            (pos.end.col + 1) +
            " - " +
            msg,
    );
    (err as any).pos = pos;
    return err;
};

type Code = Line[];
type Line = { text: string; comment: BlockComment; indent?: number };
// eg {out: "$a0", msg: "1 + 1"}
// eg {msg: "return"}
type InlineCommentPiece = InlineCommentPiece[] | OutComment | string;
type OutComment = { out?: string; msg: InlineCommentPiece };
type BlockComment = OutComment | { msg: InlineCommentPiece; out: undefined };
let nocmnt = (): BlockComment => ({ msg: [] });
// TODO:
// text: string
// comment: {out: string, value: string} | string

export function compile(srcraw: string, filename: string): string {
    let src = srcraw.split("\t").join("    ");
    const baseast = parse(src, filename) as Ast[];
    let mair = mipsgen(baseast);
    // console.log("\n\n" + mair.join("\n") + "\n\n");
    let res = finalize(mair);
    usedLoopNames = {};
    return res;
}

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
type LabelDetails = {
    def: string;
    ref: string;
    raw: string;
};
function mklabel(name: string): LabelDetails {
    let nme: string;
    if (usedLoopNames[name]) {
        nme = name + "_" + ++usedLoopNames[name];
        if (usedLoopNames[nme]) nme = mklabel(nme).raw; // yes ok this is good wcgw
    } else {
        nme = name;
    }
    usedLoopNames[name] = (usedLoopNames[name] || 0) + 1;
    return {
        def: "%%:label:" + nme + ":%%",
        ref: "%%:ref:label:" + nme + ":%%",
        raw: nme,
    };
}

// because prettier doesn't know how to format code sensibly:
// (zig fmt has no problem with this)
// prettier-ignore
let regExpansions: { [key: string]: string[] } = {
    call: [
        "v0", "v1", "a0", "a1", "a2", "a3", "t0",
        "t1", "t2", "t3", "t4", "t5", "t6", "t7",
        "ra",
    ]
};

// use from left to right, preferring left when available
// prettier-ignore
let userRegisters: string[] = [
    "t0", "t1", "t2", "t3", "t4","t5", "t6", "t7",
    "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7",
];

let matchIRRegisters = /%%:(?:out\:)?register:(.+?):%%/g;

function indent(line: Line): Line {
    return {
        text: line.text,
        comment: line.comment,
        indent: (line.indent || 0) + 1,
    };
}

function lineIsCode(line: Line): boolean {
    let ltxt = line.text.trim();
    if (!ltxt) return false;
    if (ltxt.startsWith("%:%:")) return false;
    if (ltxt.startsWith("{{")) return false;
    if (ltxt.startsWith("%:%:")) return false;
    return true;
}
function cleancode(code: Code): Code {
    return code.filter(l => lineIsCode(l));
}

type Type =
    | { type: "u32" }
    | { type: "i32" }
    // i8 cannot be added without updating load and store
    | { type: "u8" }
    | { type: "any" }
    | { type: "void" }
    | { type: "pointer"; child: Type }
    | { type: "arrayptr"; child: Type };

function evalType(tast: AstType): Type {
    if (tast.type === "builtin") return { type: tast.kind };
    if (tast.type === "pointer")
        return { type: "pointer", child: evalType(tast.child) };
    if (tast.type === "arrayptr")
        return { type: "arrayptr", child: evalType(tast.child) };
    return asun(tast);
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

type ExprRetV = { typ: Type; reg: string; cmnt: InlineCommentPiece };

// I'm keeping the    vnm    here because immediate may include comptime constants in the future
function getImmediate(_vnm: VNM, expr: AstExpr): undefined | { value: number } {
    if (expr.expr === "immediate") {
        return { value: +expr.value };
    }
    return undefined;
}

function evalExprAllowImmediate(
    vnm: VNM,
    expr: AstExpr,
    lines?: Code,
): ExprRetV {
    let imm = getImmediate(vnm, expr);
    if (imm) {
        return {
            reg: "" + imm.value,
            typ: { type: "any" },
            cmnt: "" + imm.value,
        };
    } else {
        return evalExprAnyOut(vnm, expr, lines);
    }
}

let anytype = (): Type => ({ type: "any" });

function evalExprAnyOut(vnm: VNM, expr: AstExpr, lines?: Code): ExprRetV {
    var imm = getImmediate(vnm, expr);
    if (imm && imm.value === 0)
        return { reg: genreg("zero"), typ: anytype(), cmnt: "0" };
    if (expr.expr === "register") {
        let type: Type =
            expr.register === "sp"
                ? {
                      type: "arrayptr",
                      child: { type: "any" },
                  }
                : anytype();
        return {
            reg: genreg(expr.register),
            typ: type,
            cmnt: "$" + expr.register,
        };
    } else if (expr.expr === "variable") {
        let va = vnm.get(expr.var);
        if (!va)
            throw poserr(
                expr.pos,
                "variable not found " +
                    expr.var +
                    " (at: L" +
                    expr.pos.start.line +
                    ")",
            );
        return { reg: va.tempname, typ: va.type, cmnt: expr.var };
    } else if (expr.expr === "call" && lines) {
        let ce = vnm.getfn(expr.name);
        if (!ce) throw poserr(expr.pos, "unknown fn " + expr.name);
        return ce.call(expr.args, vnm, lines);
    } else if (lines) {
        let out = gentemp();
        let exprOut = evalExpr(vnm, expr, { reg: out, name: todo() }, lines);
        return {
            typ: exprOut.type,
            reg: out,
            cmnt: exprOut.cmnt,
        };
        // TODO: now mark all the intermediate values as unused
        // or do this in a second step later
        // probably in a second step later
    } else {
        return exprNotAvailable;
    }
}

function matchTypes(ta: Type, tb: Type, pos: Pos): Type {
    if (ta.type !== "any") {
        if (tb.type === "any") return ta;
        if (tb.type === ta.type) return ta;
        throw poserr(pos, "Incompatible Types: " + ta.type + ", " + tb.type);
    }
    if (tb.type !== "any") {
        return tb;
    }
    return { type: "any" };
}

function sizeof(ta: Type): number {
    if (ta.type === "any") throw new Error("Cannot sizeof any");
    if (ta.type === "u32") return 4;
    if (ta.type === "i32") return 4;
    if (ta.type === "u8") return 1;
    if (ta.type === "void") return 0;
    if (ta.type === "pointer") return 4;
    if (ta.type === "arrayptr") return 4;
    return asun(ta);
}

function markOut(reg: string) {
    return reg.replace("%%:", "%%:out:");
}

type AOMode = "addressof" | "load" | "store";
// store a = 5;
// todo verify 5 is the right type?
// var q: i32 = a.*
// that works, store should too
function evalDerefExpr(
    outOrStoreTo: { reg: string; name: InlineCommentPiece },
    _: "=",
    dereferencingExpr: AstExpr,
    mode: AOMode,
    lines: Code,
    vnm: VNM,
): { cmnt: OutComment; type: Type } {
    const derefExpr = dereferencingExpr;
    if (
        derefExpr.expr !== "arrayindex" &&
        derefExpr.expr !== "pointer" &&
        derefExpr.expr !== "arrayindexnomul"
    )
        throw poserr(
            derefExpr.pos,
            "Expected dereferencingexpr eg .* or [i] or [+i], got " +
                derefExpr.expr,
        );

    let from =
        derefExpr.from.expr === "data"
            ? {
                  typ: evalType(derefExpr.from.type),
                  dataaddr: derefExpr.from.name,
                  cmnt: "@" + derefExpr.from.name,
              }
            : evalExprAnyOut(vnm, derefExpr.from, lines);
    if (derefExpr.expr === "pointer") {
        if (from.typ.type !== "pointer")
            throw poserr(
                derefExpr.pos,
                "Can only dereference *pointer. got " + from.typ.type,
            );
    } else if (
        derefExpr.expr === "arrayindex" ||
        derefExpr.expr === "arrayindexnomul"
    ) {
        if (from.typ.type !== "arrayptr")
            throw poserr(
                derefExpr.pos,
                "Can only index [*]pointer. got " + from.typ.type,
            );
    } else asun(derefExpr);
    if (from.typ.type !== "pointer" && from.typ.type !== "arrayptr")
        throw new Error("never");

    let sizetmp = sizeof(from.typ.child);
    let size = sizetmp;
    if (derefExpr.expr === "arrayindexnomul") size = 1;
    let siz =
        sizetmp === 4
            ? "w"
            : sizetmp === 1
            ? "b"
            : // prettier-ignore
              (() => {throw poserr(derefExpr.pos, "unsupported size " + size
                      +  "; expected 1 or 4 byte size.", ); })();
    let instr =
        mode === "addressof"
            ? "la"
            : mode === "load"
            ? "l" + siz + (sizetmp === 1 ? "u" : "")
            : mode === "store"
            ? "s" + siz
            : asun(mode);
    let marked =
        mode === "store" ? outOrStoreTo.reg : markOut(outOrStoreTo.reg);
    let ostc: InlineCommentPiece =
        mode === "store" ? [" = ", outOrStoreTo.name] : "";
    let comment: OutComment = {
        out:
            typeof outOrStoreTo.name === "string"
                ? outOrStoreTo.name
                : "~_~+_~_+~REMOVE THIS*%#&*@)$)",
        msg: nvercmnt(),
    };
    if (derefExpr.expr === "pointer") {
        comment.msg = ["&", from.cmnt, ostc];
        if ("dataaddr" in from)
            lines.push({
                text: `${instr} ${marked} ${from.dataaddr}`,
                comment,
            });
        else
            lines.push({
                text: `${instr} ${marked} (${from.reg})`,
                comment,
            });
    } else {
        let indexImmediate = getImmediate(vnm, derefExpr.index);
        if (indexImmediate) {
            let iim = indexImmediate.value;
            let offset = iim * size;
            comment.msg = [from.cmnt, "[" + indexImmediate.value + "]", ostc];
            if ("dataaddr" in from)
                lines.push({
                    text: `${instr} ${marked} ${from.dataaddr}+${offset || ""}`,
                    comment,
                });
            else
                lines.push({
                    text: `${instr} ${marked} ${offset || ""}(${from.reg})`,
                    comment,
                });
        } else {
            let index = evalExprAnyOut(vnm, derefExpr.index, lines);
            if (index.typ.type !== "u32" && index.typ.type !== "i32")
                throw poserr(derefExpr.pos, "Index must be u32 or i32");
            let tmp: { cmnt: InlineCommentPiece; reg: string };
            if (size != 1) {
                let cmnt: OutComment = {
                    out: todo(),
                    msg: [index.cmnt, " * " + size],
                };
                tmp = { cmnt: cmnt, reg: gentemp() };
                lines.push({
                    text: `mulo ${tmp.reg}, ${index.reg} ${size}`,
                    comment: cmnt,
                });
            } else {
                tmp = { cmnt: index.cmnt, reg: index.reg };
            }
            if ("dataaddr" in from) {
                comment.msg = ["(", tmp.cmnt, " + ", from.cmnt, ").*", ostc];
                lines.push({
                    text: `${instr} ${marked} ${from.dataaddr}(${tmp.reg})`,
                    comment,
                });
            } else {
                let added = gentemp();
                let addedComment: OutComment = {
                    out: todo(),
                    msg: [tmp.cmnt, " + ", from.cmnt],
                };
                lines.push({
                    text: `addu ${markOut(added)}, ${tmp.reg} ${from.reg}`,
                    comment: addedComment,
                });
                comment.msg = ["(", addedComment, ").*", ostc];
                lines.push({
                    text: `${instr} ${marked} (${added})`,
                    comment,
                });
            }
        }
    }
    return {
        type:
            mode === "addressof"
                ? from.typ
                : mode === "load"
                ? from.typ.child
                : mode === "store"
                ? from.typ.child // for typechecking help
                : asun(mode),
        cmnt: comment,
    };
}

// if an expected return value arg is passed, it might be useful
function evalExpr(
    vnm: VNM,
    expr: AstExpr,
    outraw: { reg: string; name: string },
    lines: Code,
): { type: Type; cmnt: InlineCommentPiece } {
    let out = markOut(outraw.reg);
    let outname = outraw.name;
    // run an expr and set resregister to the expr result;
    let simpleRegister = evalExprAnyOut(vnm, expr);
    if (simpleRegister !== exprNotAvailable) {
        if (simpleRegister.reg === out)
            return { type: simpleRegister.typ, cmnt: simpleRegister.cmnt };
        let setvarComment: OutComment = {
            out: outname,
            msg: simpleRegister.cmnt,
        };
        // out.name might be unnecessary if the caller sets out.name. wait that can't happen. nvm.
        lines.push({
            text: `move ${out} ${simpleRegister.reg}`,
            comment: setvarComment,
        });
        return { type: simpleRegister.typ, cmnt: setvarComment };
    } else if (expr.expr === "immediate") {
        let liComment: OutComment = { out: outname, msg: "" + expr.value };
        lines.push({
            text: `li ${out} ${expr.value}`,
            comment: liComment,
        });
        return { type: anytype(), cmnt: liComment };
    } else if (expr.expr === "op") {
        let a = evalExprAnyOut(vnm, expr.left, lines);
        let b = evalExprAllowImmediate(vnm, expr.right, lines);
        let resType = matchTypes(a.typ, b.typ, expr.pos);

        if (expr.op === "^") {
            let cmnt: OutComment;
            if (resType.type === "u8" || resType.type === "u32") {
                cmnt = { out: outname, msg: [a.cmnt, " ^ ", b.cmnt] };
                lines.push({
                    text: `xor ${out} ${a.reg} ${b.reg}`,
                    comment: cmnt,
                });
            } else {
                throw poserr(expr.pos, "unsupported type " + resType.type);
            }
            return { type: resType, cmnt };
        }

        // all must support [] and []u
        let base = ({
            "+": "add",
            "-": "sub",
            "*": "mulo",
            "/": "div",
            "%": "rem",
        } as const)[expr.op];
        let u: string;
        if (resType.type === "u32" || resType.type === "arrayptr") u = "u";
        else if (resType.type === "i32") u = "";
        else
            throw poserr(expr.pos, "Add does not support type " + resType.type);

        let cmnt: OutComment = {
            out: outname,
            msg: [a.cmnt, " " + expr.op + " ", b.cmnt],
        };

        lines.push({
            text: `${base}${u} ${out}, ${a.reg} ${b.reg}`,
            comment: cmnt,
        });

        return { type: resType, cmnt };
    } else if (expr.expr === "addressof") {
        return evalDerefExpr(outraw, "=", expr.of, "addressof", lines, vnm);
    } else if (
        expr.expr === "arrayindex" ||
        expr.expr === "pointer" ||
        expr.expr === "arrayindexnomul"
    ) {
        return evalDerefExpr(outraw, "=", expr, "load", lines, vnm);
    } else if (expr.expr === "undefined") {
        return { type: anytype(), cmnt: "undefined" };
    } else if (expr.expr === "call") {
        let ce = vnm.getfn(expr.name);
        if (!ce) throw poserr(expr.pos, "unknown fn " + expr.name);
        let callres = ce.call(expr.args, vnm, lines);
        let comment: OutComment = { out: outname, msg: callres.cmnt };
        lines.push({ text: "move " + out + ", " + callres.reg, comment });
        return { type: callres.typ, cmnt: comment };
    } else if (expr.expr === "data") {
        let type = evalType(expr.type);
        let cmnt: OutComment = { out: outname, msg: "@" + expr.name };
        if (type.type === "pointer" || type.type === "arrayptr") {
            lines.push({
                text: `la ${out}, ${expr.name}`,
                comment: cmnt,
            });
        } else {
            lines.push({
                text: `lw ${out}, ${expr.name}`,
                comment: cmnt,
            });
            // can be optimized for size to `lw out, label(number)` to avoid an add
        }
        return { type, cmnt };
    }
    throw poserr(expr.pos, "Not implemented expr: " + expr.expr);
}

type VarInfo = {
    type: Type;
    tempname: string;
};
type LoopInfo = {
    start: string;
    end: string;
};
type FnInfo = {
    call(args: AstExpr[], argvnm: VNM, res: Code): ExprRetV;
    real?: RealFnInfo;
};
type FnReturnFn = (value: Type, lines: Code, rncmt: InlineCommentPiece) => void;
type FnReturn = { outvar: string; outvarname: string; fnreturnf: FnReturnFn };
type VNM = {
    getfn: (key: string) => FnInfo | undefined;
    setfn(key: string, hndlr: FnInfo): void;
    get: (key: string) => VarInfo | undefined;
    set: (key: string, value: VarInfo) => void;
    getLoop: () => LoopInfo | undefined;
    setLoop(nv: LoopInfo | undefined): void;
    getFnReturn: () => FnReturn | undefined;
    setFnReturn(outvar: string, ovn: string, nv: FnReturnFn): void;
};
function makeVariableNameMap(parent?: VNM, rtPrntAcss: boolean = true): VNM {
    let rta = rtPrntAcss;
    let map = new Map<string, VarInfo>();
    let fns = new Map<string, FnInfo>();
    let latestLoop: LoopInfo | undefined = undefined;
    let latestFn: FnReturn | undefined = undefined;
    return {
        getfn(name) {
            return fns.get(name) || (parent ? parent.getfn(name) : undefined);
        },
        setfn(name, value) {
            // should there be a no shadowing rule also?
            if (fns.has(name)) throw new Error("fn already defined: " + name);
            fns.set(name, value);
        },
        get(key) {
            let res = map.get(key);
            if (!res && parent && rta) return parent.get(key);
            return res;
        },
        set(key, value) {
            if (map.get(key))
                throw new Error("variable already defined: " + key);
            map.set(key, value);
        },
        getLoop() {
            return latestLoop || (parent && rta ? parent.getLoop() : undefined);
        },
        setLoop(nv) {
            if (latestLoop) throw new Error("override latest loop");
            latestLoop = nv;
        },
        getFnReturn() {
            return (
                latestFn || (parent && rta ? parent.getFnReturn() : undefined)
            );
        },
        setFnReturn(a, o, nv) {
            if (latestFn) throw new Error("override latest fn");
            latestFn = { outvar: a, outvarname: o, fnreturnf: nv };
        },
    };
}

function createInlineFn(fn: FnAst, vnm: VNM) {
    let type = evalType(fn.type);
    let expctArgs = fn.args.map(arg => ({
        typ: evalType(arg.type),
        name: arg.name,
    }));
    vnm.setfn(fn.name, {
        call: (args, argvnm, reslines) => {
            let returnMark = mklabel(fn.name + "_return");

            let nvnm = makeVariableNameMap(vnm);
            let returnTemp = gentemp();
            nvnm.setFnReturn(returnTemp, "return", (result, lines, rct) => {
                matchTypes(type, result, {
                    start: { line: -1, col: -1, index: -1 },
                    end: { line: -1, col: -1, index: -1 },
                });
                lines.push({
                    text: "j " + returnMark.ref,
                    comment: {
                        msg: ["return", rct ? [" ", rct] : "", ";"],
                    },
                });
            });
            if (args.length !== expctArgs.length)
                throw poserr(args[0].pos, "wrong arg count");
            let argComments: InlineCommentPiece[] = [];
            expctArgs.forEach((expctArg, i) => {
                let arg = args[i];
                let resvar = gentemp();
                let typ = evalExpr(
                    argvnm,
                    arg,
                    { reg: resvar, name: todo() },
                    reslines,
                );
                matchTypes(typ.type, expctArg.typ, arg.pos);

                // easier than making a arrayJoin fn
                // and probably faster
                if (i !== 0) argComments.push(", ");
                argComments.push(typ.cmnt);

                nvnm.set(expctArg.name, {
                    type: expctArg.typ,
                    tempname: resvar,
                });
            });
            let callcomment: BlockComment = {
                msg: [fn.name + "(", argComments, ")"],
            };
            let mgres = mipsgen(fn.body, nvnm);
            reslines.push(
                ...mgres.map((l, i) => ({
                    text: l.text,
                    // can't put on last line because it's a label that is removed
                    comment: i == 0 ? callcomment : nocmnt(),
                    indent: l.indent,
                })),
            );
            reslines.push({
                text: returnMark.def + ":",
                comment: { msg: "}" },
            }); // todo remove unused labels
            return {
                typ: type,
                reg: returnTemp,
                cmnt: [fn.name + "(", argComments, ")"],
            };
        },
    });
}

let argNames = ["a0", "a1", "a2", "a3"];

function printType(type: Type): string {
    if (
        type.type === "u32" ||
        type.type === "i32" ||
        type.type === "u8" ||
        type.type === "void"
    ) {
        return type.type;
    }
    if (type.type === "arrayptr") {
        return "[*]" + printType(type.child);
    }
    if (type.type === "pointer") {
        return "*" + printType(type.child);
    }
    if (type.type === "any") {
        return "word";
    }
    return asun(type);
}

function createNormalFn(fn: FnAst, vnm: VNM) {
    // create a normal fn
    let returnType = evalType(fn.type);
    let expctArgs = fn.args.map(arg => ({
        typ: evalType(arg.type),
        name: arg.name,
    }));
    let startLabel = mklabel("call_" + fn.name);
    let deinitLabel = mklabel("deinit_" + fn.name);
    vnm.setfn(fn.name, {
        call: (args, argvnm, reslines): ExprRetV => {
            if (args.length !== expctArgs.length)
                throw poserr(args[0].pos, "wrong arg count");

            let argComments: InlineCommentPiece[] = [];
            expctArgs.forEach((expctArg, i) => {
                let arg = args[i];
                // evalExpr could be named better to make it clear that
                // it outputs into a register
                let argnamenm =
                    "var $" + argNames[i] + " (" + expctArgs[i].name + ")";
                let typ = evalExpr(
                    argvnm,
                    arg,
                    {
                        reg: genreg(argNames[i]),
                        name: argnamenm,
                    },
                    reslines,
                );
                if (i != 0) argComments.push(", ");
                argComments.push(typ.cmnt);
                matchTypes(typ.type, expctArg.typ, arg.pos);
            });
            // todo would prefer fn(^^, ^^^^^^^, ^^^^)
            // eg:
            // $a0 = 5;
            // $a1 = 10;
            // =>
            //    5
            //       10
            // fn(^, ^^)
            // this could be done by changing comment to
            // {out: string, value: string}
            let returnReg = genreg("v0");
            let resComment: OutComment = {
                out: returnReg,
                msg: [fn.name + "(", argComments, ")"],
            };
            reslines.push({
                text: "jal " + startLabel.ref,
                comment: resComment,
            });
            reslines.push({
                text: "%%{{MARK_CLEAR:" + regExpansions.call.join(",") + "}}%%",
                comment: nvercmnt(),
            });
            return { typ: returnType, reg: returnReg, cmnt: resComment };
        },
        real: {
            startLabel,
            deinitLabel,
            body: fn.body,
            name: fn.name,
            args: expctArgs,
            returntype: returnType,
        },
    });
}
// insertNormalFnBody
// note that when inserting a fn body, code must be register allocated before insertion. we need to know all the registers, so we need to keep mair register markers, remember them, and make sure the actual returned code doesn't have any mair markers
// also, fn bodies should not have access to outside variables (their vnm should have all the compiletime values but no runtime values. maybe add an extra continueRuntimeValues option in vnm that disables parent checking for variables?)
type RealFnInfo = {
    name: string;
    startLabel: LabelDetails;
    deinitLabel: LabelDetails;
    body: Ast[];
    args: {
        typ: Type;
        name: string;
    }[];
    returntype: Type;
};
function insertNormalFnBody(vnm: VNM, rescode: Code, fn: RealFnInfo) {
    // make a new inner vnm that does not have access
    // to outer scope variables/loops/fn returns but does
    // have access
    let ivnm = makeVariableNameMap(vnm, false);

    ivnm.setFnReturn(
        genreg("v0"),
        "return$v0",
        (result, lines, returncomment) => {
            matchTypes(result, fn.returntype, {
                start: { line: -1, col: -1, index: -1 },
                end: { line: -1, col: -1, index: -1 },
            });
            // caller should extract out v0
            lines.push({
                text: "j " + fn.deinitLabel.ref,
                comment: {
                    msg: [
                        "return",
                        returncomment ? [" ", returncomment] : "",
                        ";",
                    ],
                }, // return ^^^^^? not yet because effort
            });
        },
    );

    let argsetLines: Code = [];
    fn.args.forEach((arg, i) => {
        let tmpVar = gentemp();
        evalExpr(
            ivnm,
            {
                expr: "register",
                register: argNames[i],
                pos: "!!this should never happen!!" as any,
            },
            { reg: tmpVar, name: arg.name },
            argsetLines,
        );
        ivnm.set(arg.name, {
            tempname: tmpVar,
            type: arg.typ,
        });
    });

    let precompiledLines: Code = [];

    precompiledLines.push(...argsetLines);

    precompiledLines.push({ text: "# body", comment: nocmnt() });
    precompiledLines.push(...mipsgen(fn.body, ivnm));
    let bodyCodeAllocated = registerAllocate(precompiledLines);
    let referencedSVariables = new Set<string>();

    for (let line of bodyCodeAllocated) {
        for (let register of line.text.matchAll(matchIRRegisters)) {
            if (register[1].match(/^s[0-7]$/)) {
                referencedSVariables.add(register[1]);
            }
        }
    }

    let svars = [...referencedSVariables, "ra"];
    console.log(svars);

    // 1: save used s registers to stack
    let bodyLines: Code = [];
    if (svars.length > 0) {
        bodyLines.push({
            text: "# save used s registers to stack",
            comment: nocmnt(),
        });
        bodyLines.push({
            text: "subiu $sp, $sp, " + svars.length * 4,
            comment: { out: "$sp", msg: "&$sp[-" + svars.length + "]" },
        });
    }
    svars.forEach((svar, i) => {
        bodyLines.push({
            text: "sw $" + svar + ", " + i * 4 + "($sp)",
            comment: { msg: "$sp[" + i + "] = $" + svar },
        });
    });
    if (svars.length > 0) {
        bodyLines.push({ text: "", comment: nocmnt() });
    }

    // 2-3: save args, run fn body
    bodyLines.push(...compileAllocated(bodyCodeAllocated));

    // 4: reload s variables from stack
    if (svars.length > 0) bodyLines.push({ text: "", comment: nocmnt() });
    if (svars.length > 0)
        bodyLines.push({
            text: fn.deinitLabel.def + ":",
            comment: { msg: "cleanup:" },
            indent: -1, // hack
        });
    if (svars.length > 0)
        bodyLines.push({
            text: "# reload used s registers from stack",
            comment: nocmnt(),
        });
    svars.forEach((svar, i) => {
        bodyLines.push({
            text: "lw $" + svar + ", " + i * 4 + "($sp)",
            comment: { out: "$" + svar, msg: "$sp[" + i + "]" },
        });
    });
    if (svars.length > 0)
        bodyLines.push({
            text: "addiu $sp, $sp, " + svars.length * 4,
            comment: { out: "$sp", msg: "&$sp[" + svars.length + "]" },
        });

    let endLabel = mklabel("skip_" + fn.name);

    let headerComments: string[] = [
        "# ====================",
        "# jal " + fn.startLabel.raw + "",
        "# args:" + (fn.args.length === 0 ? " none" : ""),
        ...fn.args.map(
            (arg, i) =>
                "#    $" +
                argNames[i] +
                ": " +
                arg.name +
                " - " +
                printType(arg.typ),
        ),
        ...(
            "# return:" +
            (fn.returntype.type === "void"
                ? " none"
                : "\n#    $v0: " + printType(fn.returntype))
        ).split("\n"),
        "# ====================",
    ];
    rescode.push(
        ...headerComments.map(comment => ({
            text: comment,
            comment: nocmnt(),
        })),
    );

    // jump over fn
    if (enableEndLabel)
        rescode.push({ text: "j " + endLabel.ref, comment: nocmnt() });
    // start label
    rescode.push({
        text: fn.startLabel.def + ":",
        comment: {
            msg:
                "fn " +
                fn.name +
                "(" +
                fn.args.map(a => a.name + ": " + printType(a.typ)).join(", ") +
                ") " +
                printType(fn.returntype) +
                "{",
        },
    });
    // body code
    rescode.push(...bodyLines.map(l => indent(l)));
    // return
    rescode.push({ text: "jr $ra", comment: { msg: "}" } });
    // fn end
    rescode.push({ text: endLabel.def + ":", comment: nocmnt() });
}

function mipsgen(ast: Ast[], parentVNM?: VNM): Code {
    // find all unordered declarations (eg functions)
    // init their types and stuff

    let finalResultCode: Code = [];
    let vnm: VNM = makeVariableNameMap(parentVNM);

    // note, this is only for predeclaring. any actual code insertion
    // should happen below so it stays in-order
    for (const line of ast) {
        if (line.ast === "fn") {
            if (line.inline) {
                createInlineFn(line, vnm);
            } else {
                createNormalFn(line, vnm);
            }
        }
    }

    for (let line of ast) {
        let code: Code = [];
        if (line.ast === "ilasm") {
            code.push({ text: line.ilasm, comment: { msg: line.ilasm } });
        } else if (line.ast === "clear") {
            code.push({
                text:
                    "%%{{MARK_CLEAR:" +
                    line.registers
                        .flatMap(r => regExpansions[r] || [r])
                        .join(",") +
                    "}}%%",
                comment: nvercmnt(),
            });
        } else if (line.ast === "defvar") {
            let tempname = gentemp();
            // right now this is only one way, it would also be useful to tell evalExpr about what type we expect (if we expect one)
            let exprResultType = evalExpr(
                vnm,
                line.default,
                { reg: tempname, name: line.name },
                code,
            );
            let oc = exprResultType.cmnt as OutComment;
            let resType: Type;
            if (line.type) {
                let defType = evalType(line.type);
                resType = matchTypes(defType, exprResultType.type, line.pos); // in the future, a specified type could be optional by : if no type is specified, set the type to rest
                oc.out = "var " + oc.out + ": " + printType(resType);
            } else {
                resType = exprResultType.type;
                oc.out = "var " + oc.out;
            }
            vnm.set(line.name, { type: resType, tempname });
        } else if (line.ast === "setvar") {
            let eao = evalExprAnyOut(vnm, line.name);
            let rt = evalExpr(
                vnm,
                line.value,
                { reg: eao.reg, name: eao.cmnt.toString() },
                // *should* be a string maybe
                // sidenote: why is there a difference between set and save?
                // save a.* = 5
                // why not `a.* = 5`
                code,
            );
            // rt.out = "var name"
            // if we were doing that, but we aren't for unknown reasons.
            matchTypes(eao.typ, rt.type, line.pos);
        } else if (line.ast === "if") {
            let left = evalExprAnyOut(vnm, line.condleft, code);
            let right = evalExprAllowImmediate(vnm, line.condright, code);
            let conditionType = matchTypes(left.typ, right.typ, line.pos);
            let u: string;
            if (conditionType.type === "u32") u = "u";
            else if (conditionType.type === "u8") u = "u";
            else if (conditionType.type === "i32") u = "";
            else if (conditionType.type === "arrayptr") u = "u";
            else if (conditionType.type === "pointer") u = "u";
            else throw poserr(line.pos, "unsupported if type " + conditionType);
            // this would be much more fun to code in zig
            // I didn't want to because it would require setting up a
            //    parser though and I've already done that twice and
            //    am working on a third

            let lavjkndas = mklabel("if_end");
            let lbl = lavjkndas.ref;
            let requiresCode = true;

            let rescode = mipsgen(line.code, vnm);
            let smplcde = cleancode(rescode);
            let jincmnt: InlineCommentPiece | undefined;
            if (
                smplcde.length === 1 &&
                smplcde[0].text.trim().startsWith("j ")
            ) {
                let jumpinstr = smplcde[0].text
                    .trim()
                    .split(" ")
                    .slice(1)
                    .join(" ");
                lbl = jumpinstr; // already a ref label so ok
                // isn't it "fun" using strings instead of a real type system?
                jincmnt = smplcde[0].comment;
                requiresCode = false;
            }

            let inverse = {
                "==": "!=",
                "!=": "==",
                "<=": ">",
                "<": ">=",
                ">": "<=",
                ">=": "<",
            } as const;

            let conditionNames = {
                "!=": "bne",
                "==": "beq",
                "<": "blt",
                "<=": "ble",
                ">=": "bge",
                ">": "bgt",
            } as const;

            let condition = requiresCode
                ? inverse[line.condition]
                : line.condition;
            let condinstr = conditionNames[condition];
            let comment: BlockComment = {
                msg: [
                    "if ",
                    left.cmnt,
                    " " + line.condition + " ",
                    right.cmnt,
                    requiresCode ? " {" : [" { ", jincmnt!, " }"],
                ],
            };

            if (condition === "!=" && right.reg === "0")
                code.push({ text: `bnez ${left.reg}, ${lbl}`, comment });
            else if (condition === "==" && right.reg === "0")
                code.push({ text: `beqz ${left.reg}, ${lbl}`, comment });
            else
                code.push({
                    text: `${condinstr}${u} ${left.reg} ${right.reg}, ${lbl}`,
                    comment,
                });

            if (requiresCode) {
                code.push(...rescode.map(l => indent(l)));
                code.push({ text: lavjkndas.def + ":", comment: { msg: "}" } });
            }
        } else if (line.ast === "loop") {
            /// continueLabel = genlabel(loop_continue);
            let startLbl = mklabel("loop_start");
            let endLbl = mklabel("loop_end");

            let vctx = makeVariableNameMap(vnm);
            vctx.setLoop({ start: startLbl.ref, end: endLbl.ref });
            let rescode = mipsgen(line.code, vctx);

            code.push({ text: startLbl.def + ":", comment: { msg: "loop {" } });
            code.push({
                text: "%%{{controlflow_mark::" + startLbl.raw + "}}%%",
                comment: nvercmnt(),
            });
            code.push(...rescode.map(l => indent(l)));
            code.push({
                text: "%%{{controlflow_goto::" + startLbl.raw + "}}%%",
                comment: nvercmnt(),
            });
            code.push({ text: "j " + startLbl.ref, comment: { msg: "}" } });
            code.push({ text: endLbl.def + ":", comment: { msg: "^" } });
        } else if (line.ast === "continue") {
            let lp = vnm.getLoop();
            if (!lp) throw poserr(line.pos, "continue not in loop");
            code.push({ text: "j " + lp.start, comment: { msg: "continue;" } });
        } else if (line.ast === "break") {
            let lp = vnm.getLoop();
            if (!lp) throw poserr(line.pos, "break not in loop");
            code.push({ text: "j " + lp.end, comment: { msg: "break;" } });
        } else if (line.ast === "return") {
            let fnreturn = vnm.getFnReturn();
            if (!fnreturn) throw poserr(line.pos, "return not in fn");
            let evlxpr = evalExpr(
                vnm,
                line.returnv,
                { reg: fnreturn.outvar, name: fnreturn.outvarname },
                code,
            );
            fnreturn.fnreturnf(evlxpr.type, code, evlxpr.cmnt);
        } else if (line.ast === "fn") {
            let fni = vnm.getfn(line.name);
            if (!fni)
                throw poserr(
                    line.pos,
                    "uuh... this should never happen: " + fni,
                );
            if (!line.inline) insertNormalFnBody(vnm, code, fni.real!);
        } else if (line.ast === "expr") {
            let res = evalExprAnyOut(vnm, line.expr, code);
            if (res.typ.type !== "void")
                throw poserr(line.pos, "unused value " + res.typ);
        } else if (line.ast === "save") {
            let value = evalExprAnyOut(vnm, line.value, code);
            let rest = evalDerefExpr(
                { reg: value.reg, name: value.cmnt },
                "=",
                line.saveloc,
                "store",
                code,
                vnm,
            );
            rest.cmnt.out = undefined;
            matchTypes(value.typ, rest.type, line.pos);
        } else {
            asun(line);
        }
        finalResultCode.push(...code);
    }
    return finalResultCode;
}

function registerAllocate(rawIR: Code): Code {
    let registerNameMap: { [key: string]: string } = {};
    let controlFlowMarks: { [key: string]: number | undefined } = {};
    rawIR.forEach((line, i) => {
        let cfMarkMatch = /%%{{controlflow_mark::(.+?)}}%%/.exec(line.text);
        if (cfMarkMatch) {
            controlFlowMarks[cfMarkMatch[1]] = i;
        }
    });
    let solveVariableInternal = (
        variableID: string,
        startIndex: number,
        endIndexExclusive: number,
        finalUnavailable: Set<string>,
        visitedMarks: { [key: string]: true },
    ) => {
        let varUnavs = new Set<string>();
        let freePasses = new Set<string>();
        let unavs: { [key: string]: Set<string> | undefined } = {};
        let getUnav = (reg: string) => {
            if (unavs[reg]) return unavs[reg]!;
            unavs[reg] = new Set<string>([reg]);
            return unavs[reg]!;
        };
        let allLines: { j: number; line: string }[] = rawIR
            .slice(startIndex, endIndexExclusive)
            .map((l, i) => ({ line: l.text, j: i + startIndex }));
        let variableIsUsedLater = false;
        while (true) {
            if (allLines.length === 0) break;
            let { j, line } = allLines.shift()!;
            // console.log(variableID + "| [" + j + "]: " + line);
            // if line contains drop, add all listed registers to unavailable
            let clearMarkMatch = /%%{{MARK_CLEAR:(.+?)}}%%/.exec(line);
            if (clearMarkMatch) {
                let clrs = clearMarkMatch[1].split(",");
                clrs.forEach(clr => {
                    varUnavs.add(clr);
                    getUnav(clr).clear();
                });
                continue;
            }
            let cfRevisitMatch = /%%{{controlflow_goto::(.+?)}}%%/.exec(line);
            if (cfRevisitMatch) {
                if (visitedMarks[cfRevisitMatch[1]]) continue;
                visitedMarks[cfRevisitMatch[1]] = true;
                let revisitStart = controlFlowMarks[cfRevisitMatch[1]]!;
                allLines.unshift(
                    ...rawIR
                        .slice(revisitStart, j)
                        .map((l, i) => ({ line: l.text, j: i + revisitStart })),
                );
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
            let regs = [...line.matchAll(/%%:register:(.+?):%%/g)].map(
                q => q[1],
            );
            let outRegs = [...line.matchAll(/%%:out:register:(.+?):%%/g)].map(
                q => q[1],
            );

            if (line.includes("%%:out:variable:" + variableID + ":%%"))
                for (let inreg of regs) {
                    [...getUnav(inreg)].map(reg => {
                        if (freePasses.has(reg)) finalUnavailable.add(reg);
                        else freePasses.add(reg);
                    });
                }
            else
                for (let inreg of regs) {
                    [...getUnav(inreg)].map(reg => finalUnavailable.add(reg));
                }

            for (let outReg of outRegs) {
                // reset unav
                getUnav(outReg).delete(outReg);
                // x cannot be this reg
                varUnavs.add(outReg);
            }

            if (line.includes("%%:variable:" + variableID + ":%%")) {
                variableIsUsedLater = true;
                [...varUnavs].map(reg => finalUnavailable.add(reg));
            }

            if (line.includes("%%:out:variable:" + variableID + ":%%")) {
                // cannot assign to zero
                finalUnavailable.add("zero");
                // add new unavs
                for (let inreg of regs) {
                    getUnav(inreg).add(inreg);
                }
                // x can be anything again
                varUnavs.clear();
            }
        }
        return variableIsUsedLater;
    };
    let solveVariable = (variableID: string, startIndex: number) => {
        let unavailableRegisters = new Set<string>([]);
        let visitedMarks: { [key: string]: true } = {};
        let variableIsUsedLater = solveVariableInternal(
            variableID,
            startIndex,
            rawIR.length,
            unavailableRegisters,
            visitedMarks,
        );
        return { unavailableRegisters, variableIsUsedLater };
    };
    let registersOnlyIR: Code = [];
    let markDelete: true[] = [];
    rawIR.forEach((lineraw, i) => {
        let line = lineraw.text;
        let lintext = line.replace(
            /%%:((?:out\:)?)variable:(.+?):%%/g,
            (_, om, letr) => {
                if (registerNameMap[letr])
                    return "%%:register:" + registerNameMap[letr] + ":%%";
                let {
                    unavailableRegisters: unavailable,
                    variableIsUsedLater,
                } = solveVariable(letr, i);
                let reg = userRegisters.find(ussr => !unavailable.has(ussr));
                if (!reg) {
                    reg = "uu";
                }
                let tempReplaced = line.replace(
                    /%%:variable:(.+?):%%/g,
                    (_, q) =>
                        registerNameMap[q]
                            ? "%%:register:" + registerNameMap[q] + ":%%"
                            : _,
                );
                let moveInstruction = tempReplaced.match(
                    /^\s*move[\s,]+%%:out:variable:(.+?):%%[\s,]+%%:register:(.+?):%%\s*/,
                );
                if (moveInstruction && moveInstruction[1] === letr) {
                    console.log("Wanting to save into " + moveInstruction[2]);
                    if (!unavailable.has(moveInstruction[2])) {
                        console.log("\\ Allowed!");
                        reg = moveInstruction[2];
                        markDelete[i] = true;
                    }
                }
                let noSideEffectsStore = line.match(
                    /^\s*move[\s,]+%%:out:variable:(.+?):%%/,
                );
                if (noSideEffectsStore && noSideEffectsStore[1] === letr) {
                    if (!variableIsUsedLater) {
                        markDelete[i] = true;
                    }
                }
                // if (reg == "uu") {
                //     console.log(
                //         "\n\n================\n" +
                //             registersOnlyIR.join("\n") +
                //             "\n=============\n\n",
                //     );
                //     console.log(
                //         [...unavailable].sort().join(", ") +
                //             "\n" +
                //             userRegisters.sort().join(", "),
                //     );
                //     throw new Error("Out of registers!");
                // }
                registerNameMap[letr] = reg;
                return "%%:" + om + "register:" + reg + ":%%";
            },
        );
        registersOnlyIR.push({
            text: lintext,
            comment: lineraw.comment,
            indent: lineraw.indent,
        });
    });
    return registersOnlyIR.filter((_, i) => !markDelete[i]);
}
function compileAllocated(registersOnlyIR: Code) {
    return registersOnlyIR
        .map(line => ({
            text: line.text.replace(matchIRRegisters, (_, q) => "$" + q),
            comment: line.comment,
            indent: line.indent,
        }))
        .filter(l => !l.text.trim().startsWith("%%{{"));
}
function cleanupUnreachable(allocatedIR: Code) {
    let clean = allocatedIR;
    // remove pointless jumps (jump to label in next line)
    let unreachable = false;
    // spoiler for the future: this will remove eg macros if they are precompiled and don't have labels. it will do a lot of things. I want to rewrite this in zig. and hopefully use real datastructures instead of strings with magic in them.
    clean = clean.filter((l, i) => {
        let line = l.text.trim();
        if (line.startsWith("#")) return true;
        if (line.match(/%%:label:.+?:%%:/)) {
            unreachable = false;
        }
        if (unreachable) return false;
        let next = clean.find((q, m) => m > i && lineIsCode(q))?.text || ""; // 10/10 code here. very high quality. fast. clean. readable.
        if (line.startsWith("j ")) {
            let jloc = line.match(/%%:ref:label:(.+?):%%/);
            if (jloc) {
                // if next line is the label, remove the jump line
                if (next.includes("%%:label:" + jloc[1] + ":%%:")) return false;
            }
        }
        if (line.startsWith("jr ") || line.startsWith("j ")) {
            // remove all lines until next label (unreachable code)
            unreachable = true;
        }
        return true;
    });
    // find used labels
    let referencedLabels = new Set<string>();
    for (let line of clean) {
        let matches = line.text.matchAll(/%%:ref:label:(.+?):%%/g);
        for (let match of matches) {
            referencedLabels.add(match[1]);
        }
    }
    let refdlbls = [...referencedLabels];
    // remove unused labels and emit
    return clean
        .filter(line => {
            let isLabel = line.text.match(/%%:label:(.+?):%%:/);
            if (isLabel) {
                return refdlbls.includes(isLabel[1]);
            }
            return true;
        })
        .map(l => ({
            text: l.text.replace(/%%:(?:ref:)?label:(.+?):%%/g, "$1"),
            comment: l.comment,
            indent: l.indent,
        }));
}
function commentate(code: Code): string[] {
    type ResultItem = {
        value: string;
        assignto: string;
        used: boolean;
        indent: string;
        idxs: number[];
    };
    let resultComments: ResultItem[] = [];
    let commentSet = new Map<BlockComment, number[]>();
    let printComment = (
        cmnt: InlineCommentPiece,
        left: number,
    ): { text: string; idxs: number[] } => {
        if (typeof cmnt === "string") return { text: cmnt, idxs: [] };
        if (Array.isArray(cmnt)) {
            let idxs: number[] = [];
            return {
                text: cmnt
                    .map(q => {
                        let res = printComment(q, left);
                        left += res.text.length;
                        idxs.push(...res.idxs);
                        return res.text;
                    })
                    .join(""),
                idxs,
            };
        }
        let linesIdxs = commentSet.get(cmnt);
        if (linesIdxs) {
            let flen = 0;
            for (let i of linesIdxs) {
                resultComments[i].value =
                    " ".repeat(left) + resultComments[i].value;
                resultComments[i].used = true;
                flen = resultComments[i].value.trim().length;
            }
            commentSet.delete(cmnt);
            return { text: "^".repeat(flen), idxs: [...linesIdxs] };
        }
        let resv = printComment(cmnt.msg, left);
        if (resv.idxs.length !== 0) throw new Error("uh oh ");
        return { text: resv.text, idxs: [] };
    };
    code.forEach((line, i) => {
        let comment = line.comment;
        let indent = "    ".repeat(line.indent || 0);
        let left = comment.out ? comment.out + " = " : "";
        let prcres = printComment(comment.msg, 0);
        let fidx = resultComments.length;
        resultComments.push({
            indent,
            value: prcres.text,
            assignto: left,
            used: false,
            idxs: new Error("uh oh") as any,
        });
        if (fidx !== i) throw new Error("never");
        let idxs = [...prcres.idxs, fidx];
        commentSet.set(comment, idxs);
        resultComments[fidx].idxs = idxs;
    });

    for (let l of resultComments) {
        if (l.used) continue;
        let leftdent = " ".repeat(l.assignto.length);
        for (let idx of l.idxs) {
            if (resultComments[idx] === l) continue;
            if (!resultComments[idx].used) throw new Error("uh oh");
            resultComments[idx].value = leftdent + resultComments[idx].value;
        }
        l.value = l.assignto + l.value;
    }

    let aboveused = false;
    let rcxt: string[] = resultComments.map(l => {
        if (l.used)
            return (
                l.indent +
                l.value.replace(
                    " ",
                    aboveused ? "|" : ((aboveused = true), "."),
                )
            );
        aboveused = false;
        return l.indent + l.value;
    });

    let rescode: string[] = [];
    code.forEach((line, i) => {
        let ltx = "    ".repeat(line.indent || 0) + line.text;
        if (line.text.trim().startsWith("#")) {
            rescode.push(ltx);
        } else {
            rescode.push(ltx.padEnd(50, " ") + ("# " + rcxt[i]).trim());
        }
    });
    return rescode;
}
function finalize(rawIR: Code): string {
    let txt = cleanupUnreachable(compileAllocated(registerAllocate(rawIR)));
    return commentate(txt).join("\n");
}
