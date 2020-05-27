import { parse, Ast, AstType, AstExpr, FnAst } from "./build";

var inputCode: string = undefined as any; // wow this is bad
export function compile(srcraw: string, filename: string): string {
    let src = srcraw.split("\t").join("    ");
    inputCode = src;
    const baseast = parse(src, filename) as Ast[];
    let mair = mipsgen(baseast);
    let res = finalize(mair);
    inputCode = new Error("uh oh") as any;
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
function genlabel(name: string): string {
    if (usedLoopNames[name]) return name + "_" + ++usedLoopNames[name];
    usedLoopNames[name] = 1;
    return name + "_" + usedLoopNames[name];
}

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

let matchIRRegisters = /%%:(?:out\:)?register:(..):%%/g;

type Type =
    | { type: "u32" }
    | { type: "i32" }
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
let commentSeparator = "%%__COMMENT_SEP__%%";

type ExprRetV = { typ: Type; reg: string };

function getImmediate(vnm: VNM, expr: AstExpr): undefined | { value: number } {
    if (expr.expr === "immediate") {
        return { value: +expr.value };
    }
    return undefined;
}

function evalExprAllowImmediate(
    vnm: VNM,
    expr: AstExpr,
    lines?: string[],
): ExprRetV {
    let imm = getImmediate(vnm, expr);
    if (imm) {
        return { reg: "" + imm.value, typ: { type: "any" } };
    } else {
        return evalExprAnyOut(vnm, expr, lines);
    }
}

let anytype = (): Type => ({ type: "any" });

function evalExprAnyOut(vnm: VNM, expr: AstExpr, lines?: string[]): ExprRetV {
    if (expr.expr === "register") {
        let type: Type =
            expr.register === "sp"
                ? {
                      type: "arrayptr",
                      child: { type: "any" },
                  }
                : anytype();
        return { reg: genreg(expr.register), typ: type };
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
    if (ta.type !== "any") {
        if (tb.type === "any") return ta;
        if (tb.type === ta.type) return ta;
        throw new Error("Incompatible Types: " + ta.type + ", " + tb.type);
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
    outOrStoreTo: string,
    _: "=",
    dereferencingExpr: AstExpr,
    mode: AOMode,
    lines: string[],
    vnm: VNM,
): Type {
    const derefExpr = dereferencingExpr;
    if (derefExpr.expr !== "arrayindex" && derefExpr.expr !== "pointer")
        throw new Error(
            "Expected dereferencingexpr eg .* or [i], got " + derefExpr.expr,
        );

    let from = evalExprAnyOut(vnm, derefExpr.from, lines);
    if (derefExpr.expr === "pointer") {
        if (from.typ.type !== "pointer")
            throw new Error(
                "Can only dereference *pointer. got " + from.typ.type,
            );
    } else if (derefExpr.expr === "arrayindex") {
        if (from.typ.type !== "arrayptr")
            throw new Error("Can only index [*]pointer. got " + from.typ.type);
    } else asun(derefExpr);
    if (from.typ.type !== "pointer" && from.typ.type !== "arrayptr")
        throw "never";

    let size = sizeof(from.typ);
    let siz =
        size === 4
            ? "w"
            : size === 1
            ? "b"
            : // prettier-ignore
              (() => {throw new Error("unsupported size " + size
                      +  "; expected 1 or 4 byte size.", ); })();
    let instr =
        mode === "addressof"
            ? "la"
            : mode === "load"
            ? "l" + siz
            : mode === "store"
            ? "s" + siz
            : asun(mode);
    let marked = mode === "store" ? outOrStoreTo : markOut(outOrStoreTo);
    if (derefExpr.expr === "pointer") {
        lines.push(`${instr} ${marked} (${from.reg})`);
    } else {
        let indexImmediate = getImmediate(vnm, derefExpr.index);
        if (indexImmediate) {
            let iim = indexImmediate.value;
            let offset = iim * size;
            lines.push(`${instr} ${marked} ${offset || ""}(${from.reg})`);
        } else {
            let index = evalExprAnyOut(vnm, derefExpr.index, lines);
            if (index.typ.type !== "u32") throw new Error("Index must be u32");
            let tmp: string;
            if (size != 1) {
                tmp = gentemp();
                lines.push(`mulo ${tmp}, ${index.reg} ${size}`);
            } else {
                tmp = index.reg;
            }
            lines.push(`${instr} ${marked} (${tmp})`);
        }
    }
    return mode === "addressof"
        ? from.typ
        : mode === "load"
        ? from.typ.child
        : mode === "store"
        ? from.typ.child // for typechecking help
        : asun(mode);
}

// if an expected return value arg is passed, it might be useful
function evalExpr(
    vnm: VNM,
    expr: AstExpr,
    outraw: string,
    lines: string[],
): Type {
    let out = markOut(outraw);
    // run an expr and set resregister to the expr result;
    let simpleRegister = evalExprAnyOut(vnm, expr);
    if (simpleRegister !== exprNotAvailable) {
        if (simpleRegister.reg === out) return simpleRegister.typ;
        lines.push(`move ${out} ${simpleRegister.reg}`);
        return simpleRegister.typ;
    } else if (expr.expr === "immediate") {
        lines.push(`li ${out} ${expr.value}`);
        return anytype();
    } else if (expr.expr === "op") {
        let a = evalExprAnyOut(vnm, expr.left, lines);
        let b = evalExprAllowImmediate(vnm, expr.right, lines);
        let resType = matchTypes(a.typ, b.typ);

        if (expr.op === "^") {
            if (resType.type === "u8" || resType.type === "u32") {
                lines.push(`xor ${out} ${a.reg} ${b.reg}`);
            } else {
                throw new Error("unsupported type " + resType.type);
            }
            return resType;
        }

        // all must support [] and []u
        let base = ({
            "+": "add",
            "-": "sub",
            "*": "mulo",
            "/": "div",
            "%": "rem",
        } as const)[expr.op];

        if (resType.type === "u32" || resType.type === "arrayptr")
            lines.push(`${base}u ${out}, ${a.reg} ${b.reg}`);
        else if (resType.type === "i32")
            lines.push(`${base} ${out}, ${a.reg} ${b.reg}`);
        else throw new Error("Add does not support type " + resType.type);

        return resType;
    } else if (expr.expr === "addressof") {
        return evalDerefExpr(outraw, "=", expr.of, "addressof", lines, vnm);
    } else if (expr.expr === "arrayindex" || expr.expr === "pointer") {
        return evalDerefExpr(outraw, "=", expr, "load", lines, vnm);
    } else if (expr.expr === "undefined") {
        return anytype();
    } else if (expr.expr === "call") {
        let ce = vnm.getfn(expr.name);
        if (!ce) throw new Error("unknown fn " + expr.name);
        return ce.call(expr.args, vnm, lines);
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
type FnInfo = {
    call(args: AstExpr[], argvnm: VNM, res: string[]): Type;
    real?: RealFnInfo;
};
type FnReturnInfo = {
    return(value: AstExpr): void;
};
type VNM = {
    getfn: (key: string) => FnInfo | undefined;
    setfn(key: string, hndlr: FnInfo): void;
    get: (key: string) => VarInfo | undefined;
    set: (key: string, value: VarInfo) => void;
    getLoop: () => LoopInfo | undefined;
    setLoop(nv: LoopInfo | undefined): void;
};
function makeVariableNameMap(parent?: VNM, rtPrntAcss: boolean = true): VNM {
    let rta = rtPrntAcss;
    let map = new Map<string, VarInfo>();
    let fns = new Map<string, FnInfo>();
    let latestLoop: LoopInfo | undefined = undefined;
    let latestFn: FnReturnInfo | undefined = undefined;
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
            latestLoop = nv;
        },
    };
}

function createInlineFn(fn: FnAst, vnm: VNM) {
    let type = evalType(fn.type);
    let expctArgs = fn.args.map(arg => ({
        typ: evalType(arg.type),
        name: arg.name,
    }));
    // let returnMark = genlabel(line.name + "_return");
    vnm.setfn(fn.name, {
        call: (args, argvnm, reslines) => {
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
                let typ = evalExpr(argvnm, arg, resvar, reslines);
                matchTypes(typ, expctArg.typ);
                nvnm.set(expctArg.name, {
                    type: expctArg.typ,
                    tempname: resvar,
                });
            });
            reslines.push(
                ...mipsgen(fn.body, nvnm).map(
                    l => l.split(commentSeparator)[0],
                ),
            );
            // reslines.push(returnMark + ":"); // todo remove unused labels
            return type;
        },
    });
}

let argNames = ["a0", "a1", "a2", "a3"];

function createNormalFn(fn: FnAst, vnm: VNM) {
    // create a normal fn
    let returnType = evalType(fn.type);
    let expctArgs = fn.args.map(arg => ({
        typ: evalType(arg.type),
        name: arg.name,
    }));
    let startLabelName = genlabel(fn.name + "_call");
    vnm.setfn(fn.name, {
        call: (args, argvnm, reslines) => {
            if (args.length !== expctArgs.length)
                throw new Error("wrong arg count");

            expctArgs.forEach((expctArg, i) => {
                let arg = args[i];
                // evalExpr could be named better to make it clear that
                // it outputs into a register
                let typ = evalExpr(argvnm, arg, genreg(argNames[i]), reslines);
                matchTypes(typ, expctArg.typ);
            });
            reslines.push("jal " + startLabelName);
            reslines.push(
                "%%{{MARK_CLEAR:" + regExpansions.call.join(",") + "}}%%",
            );
            // result would be in $t0, $t1 if returning was supported;
            return returnType;
        },
        real: {
            startLabel: startLabelName,
            body: fn.body,
            name: fn.name,
            args: expctArgs,
        },
    });
}
// insertNormalFnBody
// note that when inserting a fn body, code must be register allocated before insertion. we need to know all the registers, so we need to keep mair register markers, remember them, and make sure the actual returned code doesn't have any mair markers
// also, fn bodies should not have access to outside variables (their vnm should have all the compiletime values but no runtime values. maybe add an extra continueRuntimeValues option in vnm that disables parent checking for variables?)
type RealFnInfo = {
    name: string;
    startLabel: string;
    body: Ast[];
    args: {
        typ: Type;
        name: string;
    }[];
};
function insertNormalFnBody(vnm: VNM, rescode: string[], fn: RealFnInfo) {
    // make a new inner vnm that does not have access
    // to outer scope variables/loops/fn returns but does
    // have access
    let ivnm = makeVariableNameMap(vnm, false);

    let argsetLines: string[] = [];
    fn.args.forEach((arg, i) => {
        let tmpVar = gentemp();
        evalExpr(
            ivnm,
            {
                expr: "register",
                register: argNames[i],
                pos: "!!this should never happen!!" as any,
            },
            tmpVar,
            argsetLines,
        );
        ivnm.set(arg.name, {
            tempname: tmpVar,
            type: arg.typ,
        });
    });

    let precompiledLines: string[] = [];

    if (fn.args.length > 0) precompiledLines.push("# save args");
    precompiledLines.push(...argsetLines);
    if (fn.args.length > 0) precompiledLines.push("");

    precompiledLines.push("# body");
    precompiledLines.push(...mipsgen(fn.body, ivnm));
    let bodyCodeAllocated = registerAllocate(precompiledLines);
    let referencedSVariables = new Set<string>();

    for (let line of bodyCodeAllocated) {
        for (let register of line.matchAll(matchIRRegisters)) {
            if (register[1].startsWith("s")) {
                referencedSVariables.add(register[1]);
            }
        }
    }

    let svars = [...referencedSVariables];

    // 1: save used s registers to stack
    let bodyLines: string[] = [];
    if (svars.length > 0) {
        bodyLines.push("# save used s registers to stack");
        bodyLines.push("subiu $sp, $sp, " + svars.length * 4);
    }
    svars.forEach((svar, i) => {
        bodyLines.push("sw $" + svar + ", " + i * 4 + "($sp)");
    });

    // 2-3: save args, run fn body
    bodyLines.push(...compileAllocated(precompiledLines));

    // 4: reload s variables from stack
    bodyLines.push("");
    if (svars.length > 0)
        bodyLines.push("# reload used s registers from stack");
    svars.forEach((svar, i) => {
        bodyLines.push("lw $" + svar + ", " + i * 4 + "($sp)");
    });
    bodyLines.push("addiu $sp, $sp, " + svars.length * 4);

    let endLabel = genlabel(fn.name + "_skip");
    // jump over fn
    rescode.push("j " + endLabel);
    // start label
    rescode.push(fn.startLabel + ":");
    // body code
    rescode.push(...bodyLines.map(l => "    " + l));
    // return
    rescode.push("jr $ra");
    // fn end
    rescode.push(endLabel + ":");
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
            if (line.inline) {
                createInlineFn(line, vnm);
            } else {
                createNormalFn(line, vnm);
            }
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
            if (conditionType.type === "u32") u = "u";
            else if (conditionType.type === "u8") u = "u";
            else if (conditionType.type === "i32") u = "";
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
            let fni = vnm.getfn(line.name);
            if (!fni)
                throw new Error("uuh... this should never happen: " + fni);
            if (!line.inline) insertNormalFnBody(vnm, code, fni.real!);
        } else if (line.ast === "expr") {
            let res = evalExprAnyOut(vnm, line.expr, code);
            if (res.typ.type !== "void")
                throw new Error("unused value " + res.typ);
        } else if (line.ast === "save") {
            let value = evalExprAnyOut(vnm, line.value, code);
            let rest = evalDerefExpr(
                value.reg,
                "=",
                line.saveloc,
                "store",
                code,
                vnm,
            );
            matchTypes(value.typ, rest);
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
        let skipcomments = false;
        code.some((lne, i) => {
            // distribute source code over these lines evenly
            if (lne.includes(commentSeparator)) {
                finalResultCode.push(lne);
                skipcomments = true; // do not continue past a preset comment seperator
            } else {
                let codeText = srccode[i] || "";
                finalResultCode.push(
                    lne + commentSeparator + (skipcomments ? "" : codeText),
                );
            }
        });
        // if (code.length > 0)
        //     for (let i = code.length; i < srccode.length; i++) {
        //         finalResultCode.push("" + commentSeparator + srccode[i]);
        //     }
    }
    return finalResultCode;
}

function registerAllocate(rawIR: string[]): string[] {
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
    return registersOnlyIR;
}
function compileAllocated(registersOnlyIR: string[]) {
    return registersOnlyIR
        .map(line => line.replace(matchIRRegisters, (_, q) => "$" + q))
        .filter(l => !l.trim().startsWith("%%{{"));
}
function finalize(rawIR: string[]): string {
    let txt = compileAllocated(registerAllocate(rawIR));

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
