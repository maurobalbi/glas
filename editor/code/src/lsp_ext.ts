import * as vscode from "vscode";

// Opens the virtual file that will show the syntax tree
//
// The contents of the file come from the `TextDocumentContentProvider`
export function syntaxTree(ctx: CtxInit): Cmd {
    const tdcp = new (class implements vscode.TextDocumentContentProvider {
        readonly uri = vscode.Uri.parse("rust-analyzer-syntax-tree://syntaxtree/tree.rast");
        readonly eventEmitter = new vscode.EventEmitter<vscode.Uri>();
        constructor() {
            vscode.workspace.onDidChangeTextDocument(
                this.onDidChangeTextDocument,
                this,
                ctx.subscriptions
            );
            vscode.window.onDidChangeActiveTextEditor(
                this.onDidChangeActiveTextEditor,
                this,
                ctx.subscriptions
            );
        }

        private onDidChangeTextDocument(event: vscode.TextDocumentChangeEvent) {
            if (isGleamDocument(event.document)) {
                // We need to order this after language server updates, but there's no API for that.
                // Hence, good old sleep().
                void sleep(10).then(() => this.eventEmitter.fire(this.uri));
            }
        }
        private onDidChangeActiveTextEditor(editor: vscode.TextEditor | undefined) {
            if (editor && isGleamEditor(editor)) {
                this.eventEmitter.fire(this.uri);
            }
        }

        async provideTextDocumentContent(
            uri: vscode.Uri,
            ct: vscode.CancellationToken
        ): Promise<string> {
            const rustEditor = ctx.activeRustEditor;
            if (!rustEditor) return "";
            const client = ctx.client;

            // When the range based query is enabled we take the range of the selection
            const range =
                uri.query === "range=true" && !rustEditor.selection.isEmpty
                    ? client.code2ProtocolConverter.asRange(rustEditor.selection)
                    : null;

            const params = { textDocument: { uri: rustEditor.document.uri.toString() }, range };
            return client.sendRequest(ra.syntaxTree, params, ct);
        }

        get onDidChange(): vscode.Event<vscode.Uri> {
            return this.eventEmitter.event;
        }
    })();

    ctx.pushExtCleanup(new AstInspector(ctx));
    ctx.pushExtCleanup(
        vscode.workspace.registerTextDocumentContentProvider("rust-analyzer-syntax-tree", tdcp)
    );
    ctx.pushExtCleanup(
        vscode.languages.setLanguageConfiguration("ra_syntax_tree", {
            brackets: [["[", ")"]],
        })
    );

    return async () => {
        const editor = vscode.window.activeTextEditor;
        const rangeEnabled = !!editor && !editor.selection.isEmpty;

        const uri = rangeEnabled ? vscode.Uri.parse(`${tdcp.uri.toString()}?range=true`) : tdcp.uri;

        const document = await vscode.workspace.openTextDocument(uri);

        tdcp.eventEmitter.fire(uri);

        void (await vscode.window.showTextDocument(document, {
            viewColumn: vscode.ViewColumn.Two,
            preserveFocus: true,
        }));
    };
}

export function isGleamDocument(document: vscode.TextDocument): document is GleamDocument {
    // Prevent corrupted text (particularly via inlay hints) in diff views
    // by allowing only `file` schemes
    // unfortunately extensions that use diff views not always set this
    // to something different than 'file' (see ongoing bug: #4608)
    return document.languageId === "gleam" && document.uri.scheme === "file";
}

export function isGleamEditor(editor: vscode.TextEditor): editor is GleamEditor {
    return isGleamDocument(editor.document);
}

export type GleamDocument = vscode.TextDocument & { languageId: "gleam" };
export type GleamEditor = vscode.TextEditor & { document: GleamDocument };

export function sleep(ms: number) {
    return new Promise((resolve) => setTimeout(resolve, ms));
}