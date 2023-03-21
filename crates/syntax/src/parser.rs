use crate::ast::{AstNode, SourceFile};
use crate::lexer::{GleamLexer, LexToken};
use crate::SyntaxKind::{self, *};
use crate::{Error, ErrorKind, SyntaxNode};
use rowan::{Checkpoint, GreenNode, GreenNodeBuilder, TextRange, TextSize};

const MAX_STEPS: usize = 100_000_000;
const MAX_DEPTHS: usize = 500;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Parse {
    green: GreenNode,
    errors: Vec<Error>,
}

impl Parse {
    pub fn green_node(&self) -> GreenNode {
        self.green.clone()
    }

    pub fn root(&self) -> SourceFile {
        SourceFile::cast(self.syntax_node()).unwrap()
    }

    pub fn syntax_node(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green.clone())
    }

    pub fn errors(&self) -> &[Error] {
        &self.errors
    }
}

pub fn parse_file(src: &str) -> Parse {
    assert!(src.len() < u32::MAX as usize);
    let mut tokens: Vec<_> = GleamLexer::new(src).collect();
    tokens.reverse();
    let mut p = Parser {
        tokens,
        builder: GreenNodeBuilder::default(),
        errors: Vec::new(),
        src,
        steps: 0,
        depth: 0,
    };
    parse_module(&mut p);
    Parse {
        green: p.builder.finish(),
        errors: p.errors,
    }
}

struct Parser<'i> {
    tokens: Vec<LexToken<'i>>,
    builder: GreenNodeBuilder<'static>,
    errors: Vec<Error>,
    src: &'i str,
    steps: usize,
    depth: usize,
}

impl<'i> Parser<'i> {
    fn error(&mut self, kind: ErrorKind) {
        let range = self
            .tokens
            .last()
            .map(|&LexToken { range, .. }| range)
            .unwrap_or_else(|| TextRange::empty(TextSize::from(self.src.len() as u32)));
        self.errors.push(Error { range, kind });
    }

    fn checkpoint(&mut self) -> Checkpoint {
        self.builder.checkpoint()
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        self.builder.start_node(kind.into());
    }

    fn start_node_at(&mut self, cp: Checkpoint, kind: SyntaxKind) {
        self.builder.start_node_at(cp, kind.into());
    }

    fn finish_node(&mut self) {
        self.builder.finish_node();
    }

    /// Consume the next token, including whitespaces. Panic if there is no more token.
    fn bump(&mut self) {
        let LexToken { kind, range, .. } = self.tokens.pop().unwrap();
        self.builder.token(kind.into(), &self.src[range]);
    }

    /// Same with `bump`, but override the kind.
    fn bump_with_kind(&mut self, kind: SyntaxKind) {
        let LexToken { range, .. } = self.tokens.pop().unwrap();
        self.builder.token(kind.into(), &self.src[range]);
    }

    /// Consume the next token and wrap it in an ERROR node.
    fn bump_error(&mut self) {
        self.start_node(ERROR);
        self.bump();
        self.finish_node();
    }

    /// Peek the next token, including whitespaces.
    fn peek_full(&mut self) -> Option<LexToken> {
        self.steps += 1;
        assert!(self.steps < MAX_STEPS);
        self.tokens.last().copied()
    }

    fn peek_full_non_ws(&mut self) -> Option<LexToken> {
        self.ws();
        self.peek_full()
    }

    fn at_non_ws(&mut self, kind: SyntaxKind) -> bool {
        self.peek_non_ws().map(|k| k == kind).unwrap_or(false)
    }

    fn at(&mut self, kind: SyntaxKind) -> bool {
        self.peek().map(|k| k == kind).unwrap_or(false)
    }

    /// Like `peek_full`, but only returns SyntaxKind.
    fn peek(&mut self) -> Option<SyntaxKind> {
        self.peek_full().map(|LexToken { kind, .. }| kind)
    }

    /// Consume all following whitespaces if any, and peek the next token.
    fn peek_non_ws(&mut self) -> Option<SyntaxKind> {
        self.ws();
        self.peek()
    }

    /// Get an iterator of following non-whitespace tokens.
    fn peek_iter_non_ws(&mut self) -> impl Iterator<Item = SyntaxKind> + '_ {
        self.steps += 1;
        assert!(self.steps < MAX_STEPS);
        self.tokens
            .iter()
            .rev()
            .map(|&LexToken { kind, .. }| kind)
            .filter(|k| !k.is_whitespace())
    }

    /// Consumes all following whitespaces if any.
    fn ws(&mut self) {
        while matches!(self.peek(), Some(k) if k.is_whitespace()) {
            self.bump();
        }
    }

    /// Consumes a token if the next token matches the expected one, or does nothing if not.
    /// Return whether the expected token is consumed.
    fn want(&mut self, expect: SyntaxKind) -> bool {
        if self.peek_non_ws() == Some(expect) {
            self.bump();
            true
        } else {
            self.error(ErrorKind::ExpectToken(expect));
            false
        }
    }
}

fn parse_module(p: &mut Parser) {
    p.start_node(SOURCE_FILE);
    while let Some(i) = p.peek_non_ws() {
        parse_target_group(p)
    }
    p.finish_node();
}

const VALID_TARGETS: [&str; 2] = ["javascript", "erlang"];

fn parse_target_group(p: &mut Parser) {
    p.start_node(TARGET_GROUP);
    match p.peek() {
        Some(T!["if"]) => {
            p.bump();
            if let Some(LexToken { text, kind, .. }) = p.peek_full_non_ws() {
                if !VALID_TARGETS.contains(&text) {
                    p.error(ErrorKind::ExpectedTarget);
                }
                if kind == IDENT {
                    p.start_node(TARGET);
                    p.bump();
                    p.finish_node();
                }
            }
            if p.peek_non_ws() == Some(T!["{"]) {
                p.bump();
                parse_statements(p);
                p.want(T!["}"]);
            }
        }
        Some(t) if t.can_start_statement() => parse_statements(p),
        Some(_) => {
            p.error(ErrorKind::ExpectedStatement);
            p.bump_error()
        }
        None => {}
    }

    p.finish_node();
}

fn parse_statements(p: &mut Parser) {
    // p.start_node(STATEMENTS);
    loop {
        match p.peek_non_ws() {
            Some(t) if t.can_start_statement() => {
                parse_statement(p);
                continue;
            }
            _ => {
                break;
            }
        }
    }
}

fn parse_statement(p: &mut Parser) {
    let cp = p.checkpoint();
    let is_pub = visibility_opt(p);
    match p.peek_non_ws() {
        Some(T!["const"]) => parse_module_const(p, cp),
        Some(T!["import"]) => {
            if is_pub {
                p.error(ErrorKind::UnexpectedImport);
                p.bump_error();
            } else {
                parse_import(p);
            }
        }
        Some(T!["fn"]) => {
            parse_function(p, cp);
        }
        Some(_) => p.bump(),
        None => p.error(ErrorKind::UnexpectedEof),
    }
}

fn parse_function(p: &mut Parser, cp: Checkpoint) {
    assert!(p.at(T!["fn"]));
    p.start_node(FUNCTION);
    p.bump();
    p.start_node(NAME);
    p.want(IDENT);
    p.finish_node();
    p.want(T!["("]);
    p.start_node(PARAM_LIST);
    loop {
        match p.peek_non_ws() {
            Some(T![")"]) => {
                p.bump();
                break;
            }
            Some(T![","]) => {
                p.bump();
                continue;
            }
            Some(_) => {
                p.start_node(PARAM);
                if p.peek_iter_non_ws().nth(1) == Some(IDENT) {
                  p.start_node(LABEL);
                  p.want(IDENT);
                  p.finish_node();
                } 
                p.start_node(NAME);
                p.want(IDENT);
                p.finish_node();
                parse_type_annotation_opt(p);
                p.finish_node();
                continue;
            }
            None => {
              p.error(ErrorKind::ExpectToken(T![")"]));
              break;
            }
        }
    }
    p.finish_node();
    if p.at_non_ws(T!["->"]) {
      p.bump();
      parse_type(p);
    }
    p.start_node(FN_BODY);
    p.want(T!["{"]);
    parse_exprs(p);
    p.finish_node();
    p.want(T!["}"]);
    p.finish_node();
}

fn parse_exprs(p: &mut Parser) {
}

fn parse_params_opt(p: &mut Parser) {
}

fn parse_import(p: &mut Parser) {
    assert!(p.at(T!["import"]));
    p.start_node(IMPORT);
    p.bump();
    p.start_node(IMPORT_MODULE);
    loop {
        match p.peek_non_ws() {
            Some(IDENT) => {
                p.start_node(PATH);
                p.bump();
                p.finish_node();
                if p.at(T!["/"]) {
                    p.bump();
                    continue;
                } else {
                    break;
                }
            }
            _ => {
                p.error(ErrorKind::ExpectedIdentifier);
                break;
            }
        }
    }

    if p.peek_non_ws() == Some(T!["."]) {
        parse_unqualified_imports(p);
    }

    if p.at_non_ws(T!["as"]) {
        p.bump();
        p.start_node(NAME);
        p.want(IDENT);
        p.finish_node();
    }

    p.finish_node();
    p.finish_node();
}

fn parse_unqualified_imports(p: &mut Parser) {
    assert!(p.at(T!["."]));
    p.bump();
    p.want(T!["{"]);
    loop {
        match p.peek_non_ws() {
            Some(T!["}"]) => {
                p.bump();
                break;
            }
            Some(k @ (U_IDENT | IDENT)) => {
                p.start_node(UNQUALIFIED_IMPORT);
                p.start_node(NAME);
                p.bump();
                p.finish_node();
                if p.at_non_ws(T!["as"]) {
                    p.bump();
                    p.start_node(NAME);
                    p.want(k);
                    p.finish_node();
                }
                p.finish_node();
                continue;
            }
            Some(T![","]) => {
                p.bump();
                continue;
            }
            _ => {
                p.error(ErrorKind::ExpectToken(T!["}"]));
                break;
            }
        }
    }
}

fn parse_module_const(p: &mut Parser, cp: Checkpoint) {
    assert!(p.at(T!["const"]));
    p.start_node_at(cp, MODULE_CONSTANT);
    p.bump();
    parse_name(p);
    parse_type_annotation_opt(p);
    p.want(T!["="]);
    parse_constant_value(p);
    p.finish_node();
}

fn parse_constant_value(p: &mut Parser) {
    match p.peek_non_ws() {
        Some(INTEGER | STRING | FLOAT) => {
            p.start_node(LITERAL);
            p.bump();
            p.finish_node()
        }
        Some(HASH) => parse_tuple(p),
        Some(IDENT) => {
            p.start_node(NAME_REF);
            p.bump();
            p.finish_node();
        }
        Some(_) => {
            p.error(ErrorKind::ExpectedConstantExpression);
            p.bump_error();
        }
        _ => { p.error(ErrorKind::ExpectedConstantExpression)}
    }
}

fn parse_tuple(p: &mut Parser) {
    assert!(p.at(T!["#"]));
    p.start_node(TUPLE);
    p.bump();
    p.want(T!["("]);
    loop {
        match p.peek_non_ws() {
            Some(T![")"]) => {
                p.bump();
                break;
            }
            Some(k) if k.can_start_constant_expr() => {
                parse_constant_value(p);
                continue;
            }
            Some(T![","]) => {
                p.bump();
                continue;
            }
            _ => {
                p.error(ErrorKind::ExpectToken(T![")"]));
                break;
            }
        }
    }
    p.finish_node()
}

fn visibility_opt(p: &mut Parser) -> bool {
    if p.at_non_ws(T!["pub"]) {
        p.bump();
        return true;
    }
    false
}

fn parse_name(p: &mut Parser) {
    p.ws();
    p.start_node(NAME);
    p.want(IDENT);
    p.finish_node();
}

fn parse_type_annotation_opt(p: &mut Parser) {
    match p.peek_non_ws() {
        Some(T![":"]) => {
            p.bump();
            parse_type(p);
        }
        _ => {}
    }
}

fn parse_type(p: &mut Parser) {
    match p.peek_non_ws() {
        // function
        Some(T!["fn"]) => parse_fn_type(p),
        // type variable or constructor module
        Some(IDENT) => {
            let cp = p.checkpoint();
            p.bump();

            match p.peek_non_ws() {
                Some(T!["."]) => {
                    p.start_node_at(cp, CONSTRUCTOR_TYPE);
                    p.start_node_at(cp, MODULE_NAME);
                    p.finish_node();
                    p.bump();
                    p.start_node(NAME);
                    p.want(U_IDENT);
                    p.finish_node();
                    p.finish_node();
                }
                _ => {
                    p.start_node_at(cp, VAR_TYPE);
                    p.finish_node()
                }
            }
            // could also be a Constructor Module
            // p.finish_node()
        }
        // constructor
        Some(U_IDENT) => {
            p.start_node(CONSTRUCTOR_TYPE);
            p.start_node(NAME);
            p.bump();
            p.finish_node();
            p.finish_node()
        }
        // tuple
        Some(T!("#")) => {
            parse_tuple_type(p);
        }
        Some(_) => {
            p.error(ErrorKind::ExpectedType);
            p.bump_error()
        }
        None => {}
    }
}

fn parse_tuple_type(p: &mut Parser) {
    assert!(p.at(T!["#"]));
    p.start_node(TUPLE_TYPE);
    p.bump();
    p.want(T!["("]);
    loop {
        match p.peek_non_ws() {
            Some(T![")"]) => {
                p.bump();
                break;
            }
            Some(k) if k.can_start_type() => {
                parse_type(p);
                continue;
            }
            Some(T![","]) => {
                p.bump();
                continue;
            }
            _ => {
                p.error(ErrorKind::ExpectToken(T![")"]));
                break;
            }
        }
    }
    p.finish_node()
}

fn parse_fn_type(p: &mut Parser) {
    assert!(p.at(T!("fn")));
    p.start_node(FN_TYPE);
    p.bump();
    p.want(T!("("));
    p.start_node(PARAM_LIST);
    loop {
        match p.peek_non_ws() {
            Some(T![")"]) => {
                p.bump();
                break;
            }
            Some(T![","]) => {
                p.bump();
                continue;
            }
            _ => {
                p.start_node(PARAM);
                parse_type(p);
                p.finish_node();
                continue;
            }
        }
    }
    p.finish_node();
    p.want(T!["->"]);
    parse_type(p);
    p.finish_node();
}

impl SyntaxKind {
    fn can_start_constant_expr(self) -> bool {
        matches!(self, IDENT | INTEGER | FLOAT | STRING | T!["#"] | T!["["])
    }

    fn can_start_type(self) -> bool {
        matches!(self, T!["fn"] | T!["#"] | IDENT | U_IDENT)
    }

    fn can_start_statement(self) -> bool {
        matches!(
            self,
            T!["import"] | T!["pub"] | T!["const"] | T!["fn"] | T!["type"]
        )
    }

    /// Whether this token is a separator in some syntax.
    /// We should stop at these tokens during error recovery.
    fn is_separator(self) -> bool {
        matches!(self, T![")"] | T!["]"] | T!["}"] | T!["="] | T![","])
    }
}
