import * as vscode from "vscode";
import { workspace } from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

export function activate(context: vscode.ExtensionContext) {
  client = createLanguageClient();
  // Start the client. This will also launch the server
  client.start();
  let disposable = vscode.commands.registerCommand('gleamalyzer.syntaxTree', () => {
    // The code you place here will be executed every time your command is executed

    // Display a message box to the user
    vscode.window.showInformationMessage('Hello World!');
  });

  context.subscriptions.push(disposable);
}

// this method is called when your extension is deactivated
export function deactivate(): Thenable<void> | undefined {
  return client?.stop();
}

function createLanguageClient(): LanguageClient {
  let clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "gleam" }],
    synchronize: {
      fileEvents: [
        workspace.createFileSystemWatcher("**/gleam.toml"),
        workspace.createFileSystemWatcher("**/manifest.toml"),
        workspace.createFileSystemWatcher("**/*.gleam"),
      ],
    },
  };

  let serverOptions: ServerOptions = {
    command: "/users/maurobalbi/Documents/repos/gleamalyzer/target/debug/gleamalyzer",
    // args: ["lsp"],
    transport: TransportKind.stdio,
    options: {
      env: Object.assign(process.env, {
        GLEAM_LOG: "debug",
        GLEAM_LOG_PATH: "/Users/maurobalbi/Documents/repos/gleamalyzer/log.log",
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
