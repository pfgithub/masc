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
let deepest: Point = { index: -1, line: 0, col: 0 };

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
export type AstType = P & { tex: "builtin"; kind: "u32" | "i32" | "any" };

export type AstVar =
    | (P & { expr: "variable"; var: string })
    | (P & { expr: "register"; register: string });

export type AstExpr =
    | AstVar
    | (P & { expr: "immediate"; value: string })
    | (P & { expr: "add"; left: AstExpr; right: AstExpr })
    | (P & { expr: "undefined" });

export type Ast =
    | (P & { ast: "ilasm"; ilasm: string })
    | (P & { ast: "clear"; registers: string[] })
    | (P & { ast: "defvar"; name: string; type: AstType; default: AstExpr })
    | (P & { ast: "setvar"; name: AstVar; value: AstExpr });

type EventualResult = Ast | Ast[] | AstExpr | AstType | string;
type EventualResultAnd = Ast & AstType & AstVar & AstExpr & string;

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
    tex: "builtin",
    kind: typ as any,
    pos,
});
let mkadd = (a: AstExpr, b: AstExpr, pos: Pos): AstExpr => ({
    expr: "add",
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
            or(o.ilasmlyn, o.clrlyn, o.setvarlyn, o.defvarlyn).scb(
                r => r.data.val,
            ),
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
    or("u32", "i32", "any").scb((r, pos) => mktype(r.data.val, pos)),
);

l.set(
    "expr",
    p(o.addexpr).scb(r => r[0].val),
);

l.set(
    "addexpr",
    p(
        o.noopexpr,
        star(p(_, "+", _, o.noopexpr).scb(r => r[3].val)).scb(r =>
            r.map(q => q.val),
        ),
    ).scb((r, pos) => {
        let [one, two] = [r[0].val, r[1].val];
        if (two.length === 0) return one;
        if (two.length !== 1) throw new Error("multi-part add niy");
        return mkadd(one, two[0] as any, pos);
    }),
);

l.set(
    "noopexpr",
    or(o.vorexpr, o.undefinedexpr, o.immediateexpr).scb(r => r.data.val),
);

l.set(
    "undefinedexpr",
    c("!undefined").scb((_, pos): AstExpr => ({ expr: "undefined", pos })),
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

export function parse(code: string) {
    console.log(l.print());

    let res = o.code.parse({ str: code }, { index: 0, col: 0, line: 0 });

    console.log();

    let resretv = (res as any).val || res;

    fs.writeFileSync(
        __dirname + "/code.json",
        JSON.stringify((res as any).val || res, null, " "),
        "utf-8",
    );

    console.log();
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
                "./src/test/test.lang" +
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
