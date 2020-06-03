import { parse, Ast, AstType, AstExpr, FnAst } from "./build";

let enableEndLabel = false;

var inputCode: string = undefined as any; // wow this is bad
export function compile(srcraw: string, filename: string): string {
    let src = srcraw.split("\t").join("    ");
    inputCode = src;
    const baseast = parse(src, filename) as Ast[];
    let mair = mipsgen(baseast);
    console.log("\n\n" + mair.join("\n") + "\n\n");
    let res = finalize(mair);
    inputCode = new Error("uh oh") as any;
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
    ]
};

// use from left to right, preferring left when available
// prettier-ignore
let userRegisters: string[] = [
    "t0", "t1", "t2", "t3", "t4","t5", "t6", "t7",
    "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7",
];

let matchIRRegisters = /%%:(?:out\:)?register:(.+?):%%/g;

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

type ExprRetV = { typ: Type; reg: string };

// I'm keeping the    vnm    here because immediate may include comptime constants in the future
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
    var imm = getImmediate(vnm, expr);
    if (imm && imm.value === 0) return { reg: genreg("zero"), typ: anytype() };
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

    let size = sizeof(from.typ.child);
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
            let added = gentemp();
            lines.push(`add ${markOut(added)}, ${tmp} ${from.reg}`);
            lines.push(`${instr} ${marked} (${added})`);
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
        return ce.call(expr.args, vnm, out, lines);
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
    call(args: AstExpr[], argvnm: VNM, returnTo: string, res: string[]): Type;
    real?: RealFnInfo;
};
type FnReturn = (value: AstExpr, lines: string[]) => void;
type VNM = {
    getfn: (key: string) => FnInfo | undefined;
    setfn(key: string, hndlr: FnInfo): void;
    get: (key: string) => VarInfo | undefined;
    set: (key: string, value: VarInfo) => void;
    getLoop: () => LoopInfo | undefined;
    setLoop(nv: LoopInfo | undefined): void;
    getFnReturn: () => FnReturn | undefined;
    setFnReturn(nv: FnReturn | undefined): void;
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
        setFnReturn(nv) {
            if (latestFn) throw new Error("override latest fn");
            latestFn = nv;
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
        call: (args, argvnm, returnTo, reslines) => {
            let returnMark = mklabel(fn.name + "_return");

            let nvnm = makeVariableNameMap(vnm);
            let returnTemp = gentemp(); // this should hopefully get allocated to a valid register and the move instruction should be deleted
            nvnm.setFnReturn((result, lines) => {
                let rest = evalExpr(nvnm, result, returnTemp, lines);
                matchTypes(type, rest);
                lines.push("j " + returnMark.ref);
            });
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
            reslines.push(...mipsgen(fn.body, nvnm));
            reslines.push(returnMark.def + ":"); // todo remove unused labels
            reslines.push("move " + returnTo + " " + returnTemp);
            return type;
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
    vnm.setfn(fn.name, {
        call: (args, argvnm, returnTo, reslines) => {
            if (args.length !== expctArgs.length)
                throw new Error("wrong arg count");

            expctArgs.forEach((expctArg, i) => {
                let arg = args[i];
                // evalExpr could be named better to make it clear that
                // it outputs into a register
                let typ = evalExpr(argvnm, arg, genreg(argNames[i]), reslines);
                matchTypes(typ, expctArg.typ);
            });
            reslines.push("jal " + startLabel.ref);
            reslines.push(
                "%%{{MARK_CLEAR:" + regExpansions.call.join(",") + "}}%%",
            );
            let returnReg = genreg("t0");
            reslines.push("move " + returnTo + " " + returnReg);
            return returnType;
        },
        real: {
            startLabel: startLabel,
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
    body: Ast[];
    args: {
        typ: Type;
        name: string;
    }[];
    returntype: Type;
};
function insertNormalFnBody(vnm: VNM, rescode: string[], fn: RealFnInfo) {
    // make a new inner vnm that does not have access
    // to outer scope variables/loops/fn returns but does
    // have access
    let ivnm = makeVariableNameMap(vnm, false);

    ivnm.setFnReturn((result, lines) => {
        let rest = evalExpr(ivnm, result, genreg("t0"), lines);
        matchTypes(rest, fn.returntype);
        // caller should extract out t0
        lines.push("jr $ra");
    });

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

    precompiledLines.push(...argsetLines);

    precompiledLines.push("# body");
    precompiledLines.push(...mipsgen(fn.body, ivnm));
    let bodyCodeAllocated = registerAllocate(precompiledLines);
    let referencedSVariables = new Set<string>();

    for (let line of bodyCodeAllocated) {
        for (let register of line.matchAll(matchIRRegisters)) {
            if (register[1].match(/^s[0-7]$/)) {
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
    bodyLines.push(...compileAllocated(bodyCodeAllocated));

    // 4: reload s variables from stack
    if (svars.length > 0) bodyLines.push("");
    if (svars.length > 0)
        bodyLines.push("# reload used s registers from stack");
    svars.forEach((svar, i) => {
        bodyLines.push("lw $" + svar + ", " + i * 4 + "($sp)");
    });
    if (svars.length > 0) bodyLines.push("addiu $sp, $sp, " + svars.length * 4);

    let endLabel = mklabel("skip_" + fn.name);

    rescode.push(
        "# ====================",
        "# jal " + fn.startLabel.raw + "",
        "# args:" + (fn.args.length === 0 ? " none" : ""),
        ...fn.args.map(
            (arg, i) =>
                "#   $" +
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
                : "\n#    $t0: " + printType(fn.returntype))
        ).split("\n"),
        "# ====================",
    );

    // jump over fn
    if (enableEndLabel) rescode.push("j " + endLabel.ref);
    // start label
    rescode.push(fn.startLabel.def + ":");
    // body code
    rescode.push(...bodyLines.map(l => "    " + l));
    // return
    rescode.push("jr $ra");
    // fn end
    rescode.push(endLabel.def + ":");
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
            code.push(line.ilasm);
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

            let lbl = mklabel("if_end").ref;
            let requiresCode = true;

            let rescode = mipsgen(line.code, vnm); // TODO pass in variables
            if (rescode.length === 1 && rescode[0].trim().startsWith("j ")) {
                let jumpinstr = rescode[0]
                    .trim()
                    .split(" ")
                    .slice(1)
                    .join(" ");
                lbl = jumpinstr; // already a ref label so ok
                // isn't it "fun" using strings instead of a real type system?
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

            let condition = requiresCode
                ? inverse[line.condition]
                : line.condition;
            if (condition === "!=") {
                if (right.reg === "0") code.push(`bnez ${left.reg}, ${lbl}`);
                else code.push(`bne ${left.reg} ${right.reg}, ${lbl}`);
            } else if (condition == "==") {
                if (right.reg === "0") code.push(`beqz ${left.reg}, ${lbl}`);
                else code.push(`beq ${left.reg} ${right.reg}, ${lbl}`);
            } else if (condition == "<") {
                code.push(`blt${u} ${left.reg} ${right.reg}, ${lbl}`);
            } else if (condition == "<=") {
                code.push(`ble${u} ${left.reg} ${right.reg}, ${lbl}`);
            } else if (condition == ">=") {
                code.push(`bge${u} ${left.reg} ${right.reg}, ${lbl}`);
            } else if (condition == ">") {
                code.push(`bgt${u} ${left.reg} ${right.reg}, ${lbl}`);
            } else {
                asun(condition);
            }
            if (requiresCode) {
                code.push(...rescode.map(l => "    " + l));
                code.push(lbl + ":");
            }
        } else if (line.ast === "loop") {
            /// continueLabel = genlabel(loop_continue);
            let startLbl = mklabel("loop_start");
            let endLbl = mklabel("loop_end");

            let vctx = makeVariableNameMap(vnm);
            vctx.setLoop({ start: startLbl.ref, end: endLbl.ref });
            let rescode = mipsgen(line.code, vctx);

            code.push(startLbl.def + ":");
            code.push("%%{{controlflow_mark::" + startLbl.raw + "}}%%");
            code.push(...rescode.map(l => "    " + l));
            code.push("%%{{controlflow_goto::" + startLbl.raw + "}}%%");
            code.push("j " + startLbl.ref);
            code.push(endLbl.def + ":");
        } else if (line.ast === "continue") {
            let lp = vnm.getLoop();
            if (!lp) throw new Error("continue not in loop");
            code.push("j " + lp.start);
        } else if (line.ast === "break") {
            let lp = vnm.getLoop();
            if (!lp) throw new Error("break not in loop");
            code.push("j " + lp.end);
        } else if (line.ast === "return") {
            let fnreturn = vnm.getFnReturn();
            if (!fnreturn) throw new Error("return not in fn");
            fnreturn(line.returnv, code);
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
        code.forEach((lin, i) => {
            finalResultCode.push((i === code.length - 1 ? "" : " ") + lin);
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
            .map((l, i) => ({ line: l, j: i + startIndex }));
        let variableIsUsedLater = false;
        let iterIndex = 0;
        while (true) {
            iterIndex++;
            if (allLines.length === 0) break;
            let { j, line } = allLines.shift()!;
            // console.log(variableID + "| [" + j + "]: " + line);
            // if line contains drop, add all listed registers to unavailable
            let clearMarkMatch = /%%{{MARK_CLEAR:(.+?)}}%%/.exec(line);
            if (clearMarkMatch) {
                let clrs = clearMarkMatch[1].split(",");
                clrs.forEach(clr => varUnavs.add(clr));
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
                        .map((l, i) => ({ line: l, j: i + revisitStart })),
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
                if (!variableIsUsedLater)
                    console.log(line, "`" + variableID + "`");
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
    let registersOnlyIR: string[] = [];
    let markDelete: true[] = [];
    rawIR.forEach((line, i) => {
        registersOnlyIR.push(
            line.replace(/%%:((?:out\:)?)variable:(.+?):%%/g, (_, om, letr) => {
                if (registerNameMap[letr])
                    return "%%:register:" + registerNameMap[letr] + ":%%";
                let {
                    unavailableRegisters: unavailable,
                    variableIsUsedLater,
                } = solveVariable(letr, i);
                let reg = userRegisters.find(ussr => !unavailable.has(ussr));
                if (!reg) throw new Error("Out of registers!");
                let moveInstruction = line.match(
                    /^\s*move[\s,]+%%:out:variable:(.+?):%%[\s,]+%%:register:(.+?):%%\s*/,
                );
                if (moveInstruction && moveInstruction[1] === letr) {
                    if (!unavailable.has(moveInstruction[2])) {
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
                registerNameMap[letr] = reg;
                return "%%:" + om + "register:" + reg + ":%%";
            }),
        );
    });
    return registersOnlyIR.filter((_, i) => !markDelete[i]);
}
function compileAllocated(registersOnlyIR: string[]) {
    return registersOnlyIR
        .map(line => line.replace(matchIRRegisters, (_, q) => "$" + q))
        .filter(l => !l.trim().startsWith("%%{{"));
}
function cleanupUnreachable(allocatedIR: string[]) {
    let clean = allocatedIR;
    // remove pointless jumps (jump to label in next line)
    let unreachable = false;
    // spoiler for the future: this will remove eg macros if they are precompiled and don't have labels. it will do a lot of things. I want to rewrite this in zig. and hopefully use real datastructures instead of strings with magic in them.
    clean.filter((l, i) => {
        let line = l.trim();
        if (line.match(/%%:label:.+?:%%:/)) {
            unreachable = false;
        }
        let next = clean.find((q, m) => m > i && q) || ""; // 10/10 code here. very high quality. fast. clean. readable.
        if (line.startsWith("j ")) {
            let jloc = line.match(/%%:ref:label:(.+?):%%/);
            if (jloc) {
                // remove all lines until next label (unreachable code)
                unreachable = true;
                // if next line is the label, remove the jump line
                if (next.includes("%%:label:" + jloc[1] + ":%%:")) return false;
            }
        }
        if (unreachable) return false;
        return true;
    });
    // find used labels
    let referencedLabels = new Set<string>();
    for (let line of clean) {
        let matches = line.matchAll(/%%:ref:label:(.+?):%%/g);
        for (let match of matches) {
            referencedLabels.add(match[1]);
        }
    }
    let refdlbls = [...referencedLabels];
    // remove unused labels and emit
    return clean
        .filter(line => {
            let isLabel = line.match(/%%:label:(.+?):%%:/);
            if (isLabel) {
                return refdlbls.includes(isLabel[1]);
            }
            return true;
        })
        .map(l => l.replace(/%%:(?:ref:)?label:(.+?):%%/g, "$1"));
}
function commentate(code: string[]): string[] {
    // TODO: comment separator is a pretty bad way of matching
    //       lines with source code
    // let lsplits = code.map(l => l.split(commentSeparator));
    // let maxLineLen = 12;
    // for (let [code, comment] of lsplits) {
    //     if (!comment) continue;
    //     let tsc = /^ */.exec(comment)![0].length;
    //     let rl = code.length - tsc + 1;
    //     if (rl > maxLineLen) maxLineLen = rl;
    // }
    // let resLines: string[] = [];
    // for (let [code, comment] of lsplits) {
    //     if (comment)
    //         resLines.push(code.padEnd(maxLineLen, " ") + "# " + comment);
    //     else resLines.push(code);
    // }
    return code; // commentSeparator is a waste of time rn
}
function finalize(rawIR: string[]): string {
    let txt = cleanupUnreachable(compileAllocated(registerAllocate(rawIR)));
    return commentate(txt).join("\n");
}
