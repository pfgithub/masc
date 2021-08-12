import { defineConfig } from "vite";
import solidPlugin from "vite-plugin-solid";
import WindiCSS from "vite-plugin-windicss";
import virtual from "vite-plugin-virtual";
import * as fs from "fs";

export default defineConfig({
    plugins: [
        solidPlugin(),
        WindiCSS({
            scan: {
                fileExtensions: ["html", "js", "ts", "jsx", "tsx"],
            },
        }),
        virtual({
            'virtual:samples': "export const samples = " + JSON.stringify(
                Object.fromEntries(
                    [
                        "readmexample.masc",
                        "longestsequence.masc",
                        "xorencrypt.masc",
                        "fnreturn.masc",
                        "stack.masc",
                        "gcd.masc",
                        "gcd_recursive.masc",
                    ].flatMap(fname => {
                        if(!fname.endsWith(".masc")) return [];
                        return [[fname, fs.readFileSync(__dirname+"/../src/tests/"+fname, "utf-8")]];
                    })
                )
            ) + ";",
        }),
    ],
    build: {
        target: "esnext",
        polyfillDynamicImport: false,
    },
    publicDir: "static",
});
