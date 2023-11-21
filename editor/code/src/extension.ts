import * as vscode from "vscode";
import { workspace, ExtensionMode } from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";
import * as lc from "vscode-languageclient";

let client: LanguageClient;

export type SyntaxTreeParams = {
  textDocument: lc.TextDocumentIdentifier;
};

export const syntaxTree = new lc.RequestType<SyntaxTreeParams, string, void>(
  "glas/syntaxTree"
);

export function activate(context: vscode.ExtensionContext) {
  if (vscode.extensions.getExtension("gleam.gleam")) {
        vscode.window
            .showWarningMessage(
                `You have both the glas and gleam ` +
                    "plugins enabled. Having both plugins enabled at the same time could " +
                    "cause them to not work correctly. You should disable one of them.",
                "Got it",
            )
            .then(() => {}, console.error);
    }

  client = createLanguageClient(context);
  // Start the client. This will also launch the server
  client.start();

  const uri = vscode.Uri.parse('glas-syntaxTree:' + "syntax");
  // register a content provider for the cowsay-scheme
  const myScheme = 'glas-syntaxTree';
  const syntaxTreeProvider = new class implements vscode.TextDocumentContentProvider {
    constructor() {
      vscode.workspace.onDidChangeTextDocument(
        this.onDidChangeTextDocument,
        this
      );
      vscode.window.onDidChangeActiveTextEditor(
        this.onDidChangeActiveTextEditor,
        this
      );
    }

    onDidChangeTextDocument(event: vscode.TextDocumentChangeEvent) {
      console.log('changed doc')
      if (isGleamDocument(event.document)) {
        // We need to order this after language server updates, but there's no API for that.
        // Hence, good old sleep().
        void sleep(10).then(() => this.eventEmitter.fire(this.uri));
      }
    }
    onDidChangeActiveTextEditor(editor: vscode.TextEditor | undefined) {
      if (editor && isGleamEditor(editor)) {
        this.eventEmitter.fire(this.uri);
      }
    }

    uri = uri
    // emitter and its event
    eventEmitter = new vscode.EventEmitter<vscode.Uri>();
    onDidChange = this.eventEmitter.event;

    provideTextDocumentContent(uri: vscode.Uri, ct: lc.CancellationToken) {
      // simply invoke cowsay, use uri-path as text
      const gleamEditor = vscode.window.activeTextEditor;
      if (!(gleamEditor?.document.uri.scheme == "file") || !(gleamEditor.document.languageId == "gleam")) {
        return ""
      }

      const params = { textDocument: { uri: gleamEditor.document.uri.toString() } };
      return client.sendRequest(syntaxTree, params, ct)
    }
  };
  context.subscriptions.push(vscode.workspace.registerTextDocumentContentProvider(myScheme, syntaxTreeProvider));
  context.subscriptions.push(vscode.commands.registerCommand('glas.syntaxTree', async () => {
    const doc = await vscode.workspace.openTextDocument(uri); // calls back into the provider
    await vscode.window.showTextDocument(doc, {
      viewColumn: vscode.ViewColumn.Two,
      preserveFocus: true,
      preview: false
    });
  }));

}

// this method is called when your extension is deactivated
export function deactivate(): Thenable<void> | undefined {
  return client?.stop();
}

function createLanguageClient(context: vscode.ExtensionContext): LanguageClient {
  let clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "gleam" }],
    synchronize: {
      fileEvents: [
        workspace.createFileSystemWatcher("**/gleam.toml"),
      ],
    },
  };

  const ext = process.platform === "win32" ? ".exe" : "";
  let serverOptions: ServerOptions = {
    command: process.env["__GLAS_LSP_SERVER_PATH"] || vscode.Uri.joinPath(context.extensionUri, `glas${ext}`).fsPath,
    transport: TransportKind.stdio,
    options: {
      env: Object.assign(process.env, {
        GLEAM_LOG: context.extensionMode === ExtensionMode.Development ? "info" : "error",
        // GLEAM_LOG_PATH: process.env["__GLAS_LSP_SERVER_PATH"] + "/logs.log",
        GLEAM_LOG_NOCOLOUR: "1",
      }),
    },
  };

  return new LanguageClient(
    "gleam_language_server",
    "Gleam Language Server",
    serverOptions,
    clientOptions
  );
}


export type GleamDocument = vscode.TextDocument & { languageId: "gleam" };
export type GleamEditor = vscode.TextEditor & { document: GleamDocument };

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

export function sleep(ms: number) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}