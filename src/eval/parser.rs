// src/parser.rs

use crate::parser::expression::VariableParser;
use crate::parser::{ParserCore, ValueParser, ExpressionParser, BaseParser};
use crate::{build_expression_parser};
use crate::common::ast::{Judgment, NamedExpr, NamedVar};
use crate::common::tokenizer::Token;

pub struct Parser {
    core: ParserCore,
}

build_expression_parser! {
    parser = Parser,
    var_type = NamedVar,

    primitive_parsers: [
        IntParsing,
        BoolParsing,
        NilParsing,
        GroupParsing,
        VariableParser
    ],

    unary_parsers: [
        UnaryMinusParser
    ],

    dispatch_parsers: [
        IfExprParsing,
        FunExprParsing,
        RecFunExprParsing,
        LetExprParsing,
        MatchExprParsing
    ],

    binop_chain: [
        { LtExprParsing },
        { ConsExprParsing },
        { AddExprParsing, SubExprParsing },
        { MulExprParsing },
        { AppExprParsing }
    ]
}

impl ValueParser for Parser {
    fn parse_inner_expr(&self, tokens: Vec<Token>) -> Result<NamedExpr, String>{
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
    pub fn parse(&mut self) -> Result<Judgment, String> {
        let env = if let Some(Token::Ident(_)) = self.core().peek() {
            <Self as VariableParser>::parse_env_list(self)
        } else {
            Ok(vec![])
        }?;
        self.core().expect(Token::Turnstile)?;
        let expr = self.parse_expr()?;
        //print!("{}", expr);
        if let Some(Token::Evalto) = self.core().peek() {
            while self.core().peek().is_some() && self.core().peek() != Some(&Token::EOF) {
                self.core().advance();
            }
        }
        Ok(Judgment::EvaluatesTo(env, expr))
    }
}
