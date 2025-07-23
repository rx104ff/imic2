// src/parser.rs

use crate::parser::environment::traits::EnvironmentParser;
use crate::parser::value::ValueParserDefault;
use crate::parser::{ParserCore, ValueParser, ExpressionParser, BaseParser};
use crate::{build_expression_parser};
use crate::common::ast::{Judgment, NamedExpr, NamedVar, Value};
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
        VariableParsing
    ],

    unary_parsers: [
        UnaryMinusParsing
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

impl ValueParser<NamedVar> for Parser {
    fn parse_inner_expr(&self, tokens: Vec<Token>) -> Result<NamedExpr, String>{
        let mut inner_parser = Self::new(tokens);
        inner_parser.parse_expr()
    }

    fn parse_value(&mut self) -> Result<Value<NamedVar>, String> {
        self.parse_list_value()
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
            <Self as EnvironmentParser<NamedVar>>::parse_env_list(self)
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
