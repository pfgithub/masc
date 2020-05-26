import * as fs from "fs";

type CodeRef = { str: string };

type ParseReturnSuccess = {
    error: false;
    type: string;
    val: any;
    pos: Pos;
};
type ParseReturnError = { error: true; message: string };

type ParseReturn = ParseReturnSuccess | ParseReturnError;

type ParseFN = (code: CodeRef, start: Point) => ParseReturn;

export type Point = { line: number; col: number; index: number };
export type Pos = {
    // filename: string;
    start: Point;
    end: Point;
};

type ToStringFN = () => string;
type ParsedThing = { val: EventualResultAnd; pos: Pos };
type PostFN = (
    v: ParsedThing[] & string[] & { data: ParsedThing; choiceIndex: number },
    pos: Pos,
) => EventualResult;

type Parse = {
    parse: ParseFN;
    toString: ToStringFN;
    scb: (cb: PostFN) => Parse;
};

type User = string | User[] | RegExp | Parse;
function real(user: User): Parse {
    if (typeof user === "string") return c(user);
    if (Array.isArray(user)) return p(...user);
    if (user instanceof RegExp) return regex(user);
    return user;
}

let debugPrint = false;
let deepest: Point;

let globalLevel = 0;
function mkprs(fn: ParseFN, tsfn: ToStringFN): Parse {
    let post: PostFN | undefined;
    let resqwe: Parse = {
        parse: (a, b) => {
            if (debugPrint) {
                console.log(" ".repeat(globalLevel) + "| " + resqwe.toString());
                console.log(
                    " ".repeat(globalLevel) + ":",
                    b.index,
                    a.str.substr(b.index, 10) + "",
                );
            }
            if (b.index > deepest.index) deepest = b;
            globalLevel++;
            let res = fn(a, b);
            globalLevel--;
            if (globalLevel > 5000) throw new Error("bad");
            if (debugPrint)
                if (res.error)
                    console.log(" ".repeat(globalLevel) + "e " + res.message);
                else console.log(" ".repeat(globalLevel) + "^ pass");
            return !res.error && post
                ? { ...res, val: post(res.val, res.pos) }
                : res;
        },
        toString() {
            return tsfn();
        },
        scb(np) {
            post = np;
            return resqwe;
        },
    };
    return resqwe;
}

function temporaryParser(real: () => Parse, name: string): Parse {
    return {
        parse: (a, b) => {
            if (!real) throw new Error("Missing " + name);
            return real().parse(a, b);
        },
        toString: () => (real() ? real().toString() : "[31m[" + name + "](B[m"),
        scb: () => {
            throw new Error("Cannot set cb of temporary");
        },
    };
}

function named(name: string, user: User): Parse {
    let parse = real(user);
    return {
        parse: (v, q) => {
            let r = parse.parse(v, q);
            if (r.error) r.message += " (from " + name + ")";
            return r;
        },
        toString: () => name,
        scb: parse.scb,
    };
}

function or(...user: User[]): Parse {
    let choices = user.map(v => real(v));
    return mkprs(
        (code, start) => {
            let i = 0;
            let errors: string[] = [];
            for (let choice of choices) {
                let res = choice.parse(code, start);
                if (!res.error) {
                    return {
                        type: "or",
                        error: false,
                        val: { data: res, choiceIndex: i },
                        pos: res.pos,
                    };
                }
                errors.push(res.message);
                i++;
            }
            return {
                error: true,
                message:
                    "or failed. " + errors.map(e => "(" + e + ")").join(", "),
            };
        },
        () => "( " + choices.map(c => c.toString()).join(" | ") + " )",
    );
}

function p(...user: User[]): Parse {
    let order = user.map(v => real(v));
    return mkprs(
        (code, start) => {
            let cpos = start;
            let resa: any[] = [];
            for (let orde of order) {
                let res = orde.parse(code, cpos);
                if (res.error) return res;
                cpos = res.pos.end;
                resa.push(res);
            }
            return {
                type: "p",
                error: false,
                val: resa,
                pos: { start, end: cpos },
            };
        },
        () => "( " + order.map(c => c.toString()).join(" ") + " )",
    );
}

function c(str: string): Parse {
    return mkprs(
        (code, start) => {
            let respt: Point = { ...start };
            for (let i = 0; i < str.length; i++) {
                let char = code.str[start.index + i];
                if (char !== str[i])
                    return { error: true, message: "str failed" };
                respt.col++;
                respt.index++;
                if (char == "\n") {
                    respt.line++;
                    respt.col = 0;
                }
            }
            return {
                type: "c",
                error: false,
                val: str,
                pos: { start, end: respt },
            };
        },
        () => JSON.stringify(str),
    );
}

function countDistance(start: Point, str: string): Point {
    let respt: Point = { ...start };
    for (let i = 0; i < str.length; i++) {
        let char = str[i];
        respt.col++;
        respt.index++;
        if (char == "\n") {
            respt.line++;
            respt.col = 0;
        }
    }
    return respt;
}
function regex(rgx: RegExp): Parse {
    return mkprs(
        (code, start) => {
            let stringCopy = code.str.substr(start.index); // very bad and inefficient
            let match = rgx.exec(stringCopy);
            if (!match) return { error: true, message: "match failed" };
            if (!stringCopy.startsWith(match[0]))
                throw new Error("Regex must start with ^");
            return {
                type: "regex",
                error: false,
                val: match,
                pos: { start, end: countDistance(start, match[0]) },
            };
        },
        () => rgx.toString(),
    );
}

function star(user: User): Parse {
    let repeated = real(user);
    return mkprs(
        (code, start) => {
            let cpos = start;
            let results: any[] = [];
            while (true) {
                let res = repeated.parse(code, cpos);
                if (res.error) break;
                if (res.pos.start.index === res.pos.end.index) break;
                results.push(res);
                cpos = res.pos.end;
            }
            return {
                type: "star",
                error: false,
                val: results,
                pos: { start, end: cpos },
            };
        },
        () => "*" + repeated.toString(),
    );
}

function optional(user: User): Parse {
    let choice = real(user);
    return mkprs(
        (code, start) => {
            let cpos = start;

            let res = choice.parse(code, cpos);
            if (res.error)
                return {
                    type: "star",
                    error: false,
                    val: undefined,
                    pos: { start, end: start },
                };
            return {
                type: "star",
                error: false,
                val: { item: res.val },
                pos: { start, end: res.pos.end },
            };
        },
        () => "?" + choice.toString(),
    );
}

// fn custom(cb) return {parse: cb}

function parser() {
    let parses: { [key: string]: Parse } = {};
    let printData: { name: string; parse: Parse }[] = [];
    return {
        get(name: string) {
            if (parses[name]) return parses[name];
            return temporaryParser(() => parses[name], name);
        },
        set(name: string, user: User) {
            let piece = real(user);
            parses[name] = named(name, piece);
            printData.push({ name, parse: piece });
        },
        print() {
            return printData
                .map(({ name, parse }) => name + " = " + parse.toString() + ";")
                .join("\n");
        },
    };
}

let l = parser();
function g(arg: TemplateStringsArray) {
    return l.get(arg[0]);
}
let o: { [key: string]: Parse } = new Proxy(
    {},
    {
        get: (t, p, r) => {
            if (typeof p === "string") return l.get(p);
            return Reflect.get(t, p, r);
        },
    },
);

let logcb = (r: any) => (console.log(JSON.stringify(r, null, " ")), r);

//

//

//

//

//

//

//

type P = { pos: Pos };

// types are seperate from values here because.
export type AstType =
    | (P & {
          type: "builtin";
          kind: "u32" | "i32" | "any" | "void";
      })
    | (P & { type: "pointer"; child: AstType })
    | (P & { type: "arrayptr"; child: AstType });

export type AstVar =
    | (P & { expr: "variable"; var: string })
    | (P & { expr: "register"; register: string });

export type BinOp = "+" | "-";
export type AstExpr =
    | AstVar
    | (P & { expr: "immediate"; value: string })
    | (P & { expr: "op"; op: BinOp; left: AstExpr; right: AstExpr })
    | (P & { expr: "call"; name: string; args: AstExpr[] })
    | (P & { expr: "undefined" })
    | (P & { expr: "arrayindex"; from: AstExpr; index: AstExpr })
    | (P & { expr: "pointer"; from: AstExpr })
    | (P & { expr: "addressof"; of: AstExpr });

export type AstArg = P & { arg: "arg"; name: string; type: AstType };

export type FnAst = P & {
    ast: "fn";
    name: string;
    inline: boolean;
    args: AstArg[];
    body: Ast[];
    type: AstType;
};

export type Ast =
    | (P & { ast: "ilasm"; ilasm: string })
    | (P & { ast: "clear"; registers: string[] })
    | (P & { ast: "defvar"; name: string; type: AstType; default: AstExpr })
    | (P & { ast: "setvar"; name: AstVar; value: AstExpr })
    | (P & {
          ast: "if";
          condleft: AstExpr;
          condition: "==" | "!=" | "<=" | "<" | ">" | ">=";
          condright: AstExpr;
          code: Ast[];
      })
    | (P & { ast: "loop"; code: Ast[] })
    | (P & { ast: "break" })
    | (P & { ast: "continue" })
    | FnAst
    | (P & { ast: "expr"; expr: AstExpr });

type EventualResult =
    | Ast
    | Ast[]
    | AstExpr
    | AstType
    | AstArg
    | AstExpr[]
    | ((a: AstExpr) => AstExpr)
    | string;
type EventualResultAnd = Ast &
    AstType &
    AstVar &
    AstExpr &
    AstArg &
    AstExpr[] &
    ((a: AstExpr) => AstExpr) &
    string;

//

//

//

//

//

//

l.set(
    "_",
    star(
        or(
            /^\s+/, // any whitespace
            /^\/\/.*/, // slashslash comment until next non-. character
            /^\/\*[\s\S]+?\*\//, // inline comment
        ),
    ).scb(() => null as any),
);
let _ = o._;
let _req = _; // TODO make this require at least one character

//

let mktype = (typ: string, pos: Pos): AstType => ({
    type: "builtin",
    kind: typ as any,
    pos,
});
let mkbinexpr = (chce: BinOp, a: AstExpr, b: AstExpr, pos: Pos): AstExpr => ({
    expr: "op",
    op: chce,
    left: a,
    right: b,
    pos,
});
let mkvar = (strk: string, pos: Pos): AstVar => ({
    expr: "variable",
    var: strk,
    pos,
});
let mkreg = (strk: string, pos: Pos): AstVar => ({
    expr: "register",
    register: strk,
    pos,
});

let mkilasm = (ilasm: string, pos: Pos): Ast => ({ ast: "ilasm", ilasm, pos });
let mkclear = (regs: string[], pos: Pos): Ast => ({
    ast: "clear",
    registers: regs,
    pos,
});

let mksetvar = (varibl: AstVar, val: AstExpr, pos: Pos): Ast => ({
    ast: "setvar",
    name: varibl,
    value: val,
    pos,
});
let mkdefvar = (
    varibl: string,
    type: AstType,
    deflt: AstExpr,
    pos: Pos,
): Ast => ({
    ast: "defvar",
    name: varibl,
    type,
    default: deflt,
    pos,
});

//

//

// add support for @""?
l.set(
    "identifier",
    regex(/^[a-zA-Z_\x7f-\uffff][a-zA-Z0-9_\x7f-\uffff]*/).scb(r => r[0]),
);

// blanklineexpression? that could be nice
l.set(
    "code",
    // an orchain might be a nice builtin to have
    star(
        p(
            _,
            or(
                o.ilasmlyn,
                o.clrlyn,
                o.iflyn,
                o.looplyn,
                o.breaklyn,
                o.continuelyn,
                o.fnlyn,
                o.calllyn,
                o.setvarlyn,
                o.defvarlyn,
            ).scb(r => r.data.val),
            _,
        ).scb(r => r[1].val),
    ).scb(r => r.map(q => q.val)),
);

l.set(
    "ilasmlyn",
    regex(/^\\{1,2}([^\r\n]*)(?:\r?\n|$)/).scb((r, pos) => mkilasm(r[1], pos)),
);

l.set(
    "clrlyn",
    p("!clear", _req, "$", o.identifier, _, ";").scb((r, pos) =>
        mkclear([r[3].val], pos),
    ),
);

// there is no operator == so this is fine
l.set(
    "iflyn",
    p(
        "if",
        /*lockin*/ _,
        o.expr,
        _,
        or("==", "!=", "<=", "<", ">", ">=").scb(r => r.data.val),
        _,
        o.expr,
        _,
        "{",
        o.code,
        "}",
    ).scb((r, pos) => ({
        ast: "if",
        condleft: r[2].val,
        condition: r[4].val as any,
        condright: r[6].val,
        code: (r[9].val as any) as Ast[],
        pos,
    })),
);
l.set(
    "fnlynarg",
    p(o.identifier, _, ":", _, o.type).scb((r, pos) => ({
        arg: "arg",
        name: r[0].val,
        type: r[4].val,
        pos: pos,
    })),
);
l.set(
    "fnlyn",
    p(
        optional("inline"),
        _, // req(_)
        "fn",
        _, // req(_)
        o.identifier,
        _,
        "(",
        _,
        star(p(o.fnlynarg, _, ",", _).scb(r => r[0].val)).scb(r =>
            r.map(q => q.val),
        ),
        optional(o.fnlynarg),
        _,
        ")",
        _,
        o.type,
        _,
        "{",
        o.code,
        "}",
    ).scb((r: any, pos) => ({
        ast: "fn",
        name: r[4].val,
        inline: !!r[0].val,
        args: r[9].val ? [...r[8].val, r[9].val.item] : r[8].val,
        body: r[16].val,
        type: r[13].val,
        pos,
    })),
);
l.set(
    "looplyn",
    p("loop", /*lockin*/ _, "{", o.code, "}").scb((r, pos) => ({
        ast: "loop",
        code: (r[3].val as any) as Ast[],
        pos,
    })),
);
l.set(
    "breaklyn",
    p("break", /*lockin*/ _, ";").scb((r, pos) => ({
        ast: "break",
        pos,
    })),
);
l.set(
    "continuelyn",
    p("continue", /*lockin*/ _, ";").scb((r, pos) => ({
        ast: "continue",
        pos,
    })),
);

l.set(
    "setvarlyn",
    p(o.vorexpr, _, "=", _, o.expr, _, ";").scb((r, pos) =>
        mksetvar(r[0].val, r[4].val, pos),
    ),
);
l.set(
    "defvarlyn",
    p(
        "var",
        _req,
        o.identifier,
        _,
        ":",
        _,
        o.type,
        _,
        "=",
        _,
        o.expr,
        _,
        ";",
    ).scb((r, pos) => mkdefvar(r[2].val, r[6].val, r[10].val, pos)),
);

l.set(
    "vorexpr",
    or(o.varexpr, o.regexpr).scb(r => r.data.val),
);

l.set(
    "varexpr",
    p(o.identifier).scb((r, pos) => mkvar(r[0].val, pos)),
);
l.set(
    "regexpr",
    p("$", o.identifier).scb((r, pos) => mkreg(r[1].val, pos)),
);

// no conflicts with variable names because types are not values (same way typescript prevents this issue)
l.set(
    "type",
    or(
        or("u32", "i32", "any", "void").scb((r, pos) =>
            mktype(r.data.val, pos),
        ),

        p("[*]", _, o.type).scb(
            (r, pos): AstType => ({ type: "arrayptr", child: r[2].val, pos }),
        ),
        p("*", _, o.type).scb(
            (r, pos): AstType => ({ type: "pointer", child: r[2].val, pos }),
        ),
    ).scb(r => r.data.val),
);

l.set(
    "expr",
    p(o.addexpr).scb(r => r[0].val),
);

l.set(
    "addexpr",
    p(
        o.prefixexpr,
        star(
            p(
                _,
                or("+", "-").scb(r => r.data.val),
                _,
                o.prefixexpr,
            ).scb(r => ({ op: r[1].val, val: r[3].val } as any)),
        ).scb(r => r.map(q => q.val)),
    ).scb((r, pos) => {
        let [one, two] = [r[0].val, r[1].val];
        if (two.length === 0) return one;
        if (two.length !== 1) throw new Error("multi-part add niy");
        let twoz = two[0] as any;

        return mkbinexpr(twoz.op, one, twoz.val, pos);
    }),
);

// a.b().c() doesn't exist, so this
// is just identifier(args,)
l.set(
    "callexpr",
    p(
        o.identifier,
        _,
        "(",
        _,
        star(p(o.expr, _, ",", _).scb(q => q[0].val)).scb(q =>
            q.map(m => m.val),
        ),
        optional(o.expr),
        _,
        ")",
    ).scb(
        (r: any, pos): AstExpr => ({
            expr: "call",
            name: r[0].val,
            args: r[5].val ? [...r[4].val, r[5].val.item] : r[4].val,
            pos,
        }),
    ),
);
l.set(
    "calllyn",
    p(o.callexpr, _, ";").scb((r, pos) => ({
        ast: "expr",
        expr: r[0].val,
        pos,
    })),
);

l.set(
    "indexsuffix",
    p("[", _, o.expr, _, "]", _).scb((m, pos) => (q: AstExpr): AstExpr => ({
        expr: "arrayindex",
        from: q,
        index: m[2].val,
        pos,
    })),
);

l.set(
    "ptrfollowsuffix",
    p(".*", _).scb((m, pos) => (q: AstExpr): AstExpr => ({
        expr: "pointer",
        from: q,
        pos,
    })),
);

l.set(
    "addressofexpr",
    p("&", _, o.prefixexpr).scb((r, pos) => ({
        expr: "addressof",
        of: r[2].val,
        pos,
    })),
);

l.set(
    "prefixexpr",
    or(o.addressofexpr, o.suffixexpr).scb(r => r.data.val),
);

l.set(
    "suffixexpr",
    p(o.noopexpr, _, star(or(o.indexsuffix, o.ptrfollowsuffix))).scb(m => {
        let resexpr = m[0].val;
        for (let itm of m[2].val as any[]) {
            let suffixfn = itm.val.data.val;
            resexpr = suffixfn(resexpr);
        }
        return resexpr;
    }),
);

l.set(
    "noopexpr",
    or(o.callexpr, o.vorexpr, o.undefinedexpr, o.immediateexpr).scb(
        r => r.data.val,
    ),
);

l.set(
    "undefinedexpr",
    c("undefined").scb((_, pos): AstExpr => ({ expr: "undefined", pos })),
);
l.set(
    "immediateexpr",
    regex(/^\d+/).scb(
        (kd, pos): AstExpr => ({ expr: "immediate", value: kd[0], pos }),
    ),
);

//

//

//

//

//

//

//

//

//

//

//

//

//

//

//

//

//

//

// l.set("implicitCtxExpression") // idk

console.log(l.print());
export function parse(code: string, filename: string) {
    deepest = { index: -1, line: 0, col: 0 };

    let res = o.code.parse({ str: code }, { index: 0, col: 0, line: 0 });

    console.log();

    let resretv = (res as any).val || res;

    fs.writeFileSync(
        __dirname + "/code.json",
        JSON.stringify((res as any).val || res, null, " "),
        "utf-8",
    );

    if (deepest.index !== code.length) {
        console.log(deepest);
        let tensp = " ".repeat(10);
        let lnum = deepest.line + 1;
        let cnum = deepest.col + 1;
        let pfxsp = "      " + " ".repeat(lnum.toString().length);
        let fwidth = (process.stdout.columns || 80) - pfxsp.length - 1;
        let qwidth = fwidth - 10;
        let region = (tensp + code + " ".repeat(process.stdout.columns))
            .substr(deepest.index, fwidth)
            .split("\n")
            .join("⏎");
        let colors = {
            error: "[31m",
            green: "[32m",
            number: "[36m",
            clear: "(B[m",
            linedisp: "[97m[40m",
            linenodisp: "[30m[47m",
        };
        console.log(
            colors.green +
                filename +
                colors.clear +
                ":" +
                colors.number +
                lnum +
                colors.clear +
                ":" +
                colors.number +
                cnum +
                colors.clear +
                ": " +
                colors.error +
                "Error" +
                colors.clear,
        );
        console.log(
            colors.linenodisp + " " + lnum + "  " + colors.linedisp + "  ",
            region + colors.clear + "…",
        );
        console.log(
            pfxsp +
                colors.error +
                "          " +
                "~".repeat(qwidth) +
                colors.clear,
        );
        process.exit(1);
    }
    console.log();

    return resretv;
}
