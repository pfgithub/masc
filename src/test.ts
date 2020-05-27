import * as fs from "fs";
import * as path from "path";

import { compile } from "./conv";

let update = process.argv.includes("--update");

let onetest = process.argv.find((q, i) => i > 1 && !q.startsWith("--"));

let tests = onetest ? [onetest] : fs.readdirSync("src/tests");
let pass = true;
for (let testfylname of tests) {
    if (!testfylname.endsWith(".masc")) continue;
    let testnme = testfylname.substr(0, testfylname.lastIndexOf("."));
    let testfyl = path.join("src/tests", testfylname);
    let testdir = testfyl.substr(0, testfyl.lastIndexOf("."));
    try {
        fs.mkdirSync(testdir);
    } catch {}

    let inputCode = fs.readFileSync(testfyl, "utf-8");
    let mipsRes = compile(inputCode, testfyl);

    let expctResFile = path.join(testdir, testnme + ".mips");
    let expectedOutput: string;
    try {
        expectedOutput = fs.readFileSync(expctResFile, "utf-8");
    } catch {
        fs.writeFileSync(expctResFile, mipsRes, "utf-8");
        continue;
    }
    if (update) fs.writeFileSync(expctResFile, mipsRes, "utf-8");
    else if (expectedOutput != mipsRes) {
        pass = false;
        console.log("==== GOT ====");
        console.log(mipsRes);
        console.log("=============");
    }
}
if (!pass) {
    console.log("tests failed.");
    process.exit(1);
}
