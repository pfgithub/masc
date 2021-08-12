import { basicSetup } from "@codemirror/basic-setup";
import { indentWithTab } from "@codemirror/commands";
import { Compartment, EditorState } from "@codemirror/state";
import { EditorView, keymap } from "@codemirror/view";
import { createEffect, createSignal, JSX, onCleanup, Show, Switch, Match, For } from "solid-js";
import { render } from "solid-js/web";
import {samples} from "virtual:samples";
import "virtual:windi.css";
import { compile, CompileRes, parser_spec } from "../../src/conv";

const tab_size = new Compartment();

function Code(props: {initial_text: string, onInput: (text: string) => void}): JSX.Element {
    const state = EditorState.create({
        extensions: [
            basicSetup,
            keymap.of([indentWithTab]),
            EditorView.updateListener.of(v => {
                if(v.docChanged) {
                    props.onInput(v.state.doc.toString());
                }
            }),
            tab_size.of(EditorState.tabSize.of(4)),
        ],
    });

    return <div ref={el => {
        const view = new EditorView({
            state,
            parent: el,
        });

        createEffect(() => {
            view.update([view.state.update({
                changes: {from: 0, to: view.state.doc.length, insert: props.initial_text},
            })]);
        });

        onCleanup(() => view.destroy());
    }}></div>;
}

function Editor(): JSX.Element {
    const [error, setError] = createSignal<string | null>(null);
    const [compiled, setCompiled] = createSignal<CompileRes | null>(null);
    const [view, setView] = createSignal<string>("compiled");
    const [sample, setSample] = createSignal<string>("readmexample.masc");
    const initialText = () => {
        const res = samples[sample()] ?? "Error. Invalid sample.";
        return "// " + (sample() || "gcd") + "\n\n" + res;
    };
    const [text, setText] = createSignal(initialText());

    createEffect(() => {
        setText(initialText());
    });

    createEffect(() => {
        const source = text();
        console.log("effect called.");
        try {
            setCompiled(compile(source, "demo.masc"));
            setError(null);
        }catch(e) {
            console.log(e);
            setError((e.toString()) ?? "Error. Check Console");
        }
    });

    return <div class="grid sm:grid-cols-2 gap-4">
        <div class="sm:border-r-1">
            <select class="border-2" value={sample()} onInput={nv => {
                if(text() !== initialText()) {
                    if(!confirm("Delete edits? Ok = Delete, Cancel = Keep")) {
                        nv.currentTarget.value = sample();
                        return;
                    }
                }
                setSample(nv.currentTarget.value);
            }}>
                <option value="readmexample.masc">Samples</option>
                <For each={Object.entries(samples)}>{([name]) => (
                    <option value={name}>{name}</option>
                )}</For>
            </select>
            <Code initial_text={initialText()} onInput={newtext => {
                console.log("text changed.");
                setText(newtext);
            }} />
        </div>
        <aside class="overflow-scroll sm:border-l-1">
            <select class="border-2" value={view()} onInput={nv => setView(nv.currentTarget.value)}>
                <option value="compiled">Compiled Code</option>
                <option value="intermediate">Intermediate Representation</option>
                <option value="ast">AST</option>
                <option value="parser_spec">Parser Spec</option>
            </select>
            <div class="relative text-sm p-4">
                <Show when={error()}>{etxt => (
                    <pre class="absolute bg-red-300">{etxt}</pre>
                )}</Show>
                <Switch>
                    <Match when={view() === "compiled"}>
                        <pre>{compiled()?.final}</pre>
                    </Match>
                    <Match when={view() === "parser_spec"}>
                        <pre>{parser_spec}</pre>
                    </Match>
                    <Match when={view() === "intermediate"}>
                        <pre>{compiled()?.intermediate.map(line => {
                            return ("    ".repeat(line.indent ?? 0)) + line.text;
                        }).join("\n").replace(
                            /%%{{MARK_CLEAR:(.+?)}}%%/g, "!clear $1",
                        ).replace(
                            /%%:register:(.+?):%%/g, "$$$1",
                        ).replace(
                            /%%:out:register:(.+?):%%/g, "$$$1=",
                        ).replace(
                            /%%:variable:(.+?):%%/g, "#$1",
                        ).replace(
                            /%%:out:variable:(.+?):%%/g, "#$1=",
                        ).replace(
                            /%%:(?:ref:)?label:(.+?):%%/g, "<$1>",
                        )}</pre>
                    </Match>
                    <Match when={view() === "ast"}>
                        <pre>{JSON.stringify(compiled()?.ast, (k, v) => {
                            if(k === "pos") {
                                return "%<%" + (v.start.line + 1)
                                +":"+(v.start.col + 1)
                                +"-"+(v.end.line + 1)
                                +":"+(v.end.col + 1) + "%>%";
                            }
                            return v;
                        }, "    ").replaceAll("\"%<%", "").replaceAll("%>%\"", "")}</pre>
                    </Match>
                </Switch>
            </div>
        </aside>
    </div>;
}

render(() => <Editor />, document.getElementById("app")!);
