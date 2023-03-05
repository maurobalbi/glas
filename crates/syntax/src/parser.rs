use crate::ast::{AstNode, Module};
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

    pub fn root(&self) -> Module {
        Module::cast(self.syntax_node()).unwrap()
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
    /// Parse the whole source file.
    fn parse(mut self) -> Parse {
        self.start_node(MODULE);
        // self.expr_function_opt();
        // while self.peek_non_ws().is_some() {
        //     // Tolerate multiple exprs and just emit errors.
        //     self.error(ErrorKind::MultipleRoots);

        //     let prev = self.tokens.len();
        //     self.expr_function_opt();
        //     // Don't stuck.
        //     if self.tokens.len() == prev {
        //         self.bump_error();
        //     }
        // }
        self.finish_node();

        Parse {
            green: self.builder.finish(),
            errors: self.errors,
        }
    }

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
        let LexToken { kind, range, .. } = self.tokens.pop().unwrap();
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

    fn at(&mut self, kind: SyntaxKind) -> bool {
      self.peek().map(|k| k == kind).unwrap_or(false)
    }

    /// Like `peek_full, but only returns SyntaxKind.
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
    p.start_node(MODULE);
    match p.peek_non_ws() {
      Some(T!["if"]) => {
        parse_target_group(p);
      }
      _ => {
        parse_statements(p)
      }
    }
    // self.expr_function_opt();
    // while self.peek_non_ws().is_some() {
    //     // Tolerate multiple exprs and just emit errors.
    //     self.error(ErrorKind::MultipleRoots);

    //     let prev = self.tokens.len();
    //     self.expr_function_opt();
    //     // Don't stuck.
    //     if self.tokens.len() == prev {
    //         self.bump_error();
    //     }
    // }
    p.finish_node();
}

fn parse_target_group(p: &mut Parser) {
  assert!(p.at(T!["if"]));
  p.start_node(TARGET_GROUP);
  p.bump();
  p.finish_node();
}

fn parse_statements(p: &mut Parser) {
  p.start_node(STATEMENTS);
  p.finish_node();
}