use lsp_types::request::Request;
use lsp_types::TextDocumentIdentifier;
use serde::{Deserialize, Serialize};

pub enum SyntaxTree {}

impl Request for SyntaxTree {
    type Params = SyntaxTreeParams;
    type Result = String;
    const METHOD: &'static str = "glas/syntaxTree";
}

#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct SyntaxTreeParams {
    pub text_document: TextDocumentIdentifier,
}
