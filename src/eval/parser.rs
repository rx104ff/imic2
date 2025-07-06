// src/parser.rs

use crate::common::ast::{Expr, Judgment};
use crate::common::parser::{ExpressionParser, ParserCore, ValueParser};
use crate::common::tokenizer::Token;

pub struct Parser {
    core: ParserCore,
}

impl ExpressionParser for Parser {
    fn core(&mut self) -> &mut ParserCore {
        &mut self.core
    }
}

impl ValueParser for Parser {
    fn parse_inner_expr(&self, tokens: Vec<Token>) -> Result<Expr, String> {
        let mut inner_parser = Self::new(tokens);
        inner_parser.parse_expr()
    }
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { core: ParserCore::new(tokens) }
    }
    
    /// The unique entry point for the `eval` parser.
    /// It parses a judgment of the form `Γ ⊢ e evalto v`.
    pub fn parse_program(&mut self) -> Result<Judgment, String> {
        let env = if let Some(Token::Ident(_)) = self.core().peek() {
            self.parse_env_list()
        } else {
            Ok(vec![])
        }?;
        self.core().expect(Token::Turnstile)?;
        let expr = self.parse_expr()?;
        if let Some(Token::Evalto) = self.core().peek() {
            while self.core().peek().is_some() && self.core().peek() != Some(&Token::EOF) {
                self.core().advance();
            }
        }
        Ok(Judgment::EvaluatesTo(env, expr))
    }
}
