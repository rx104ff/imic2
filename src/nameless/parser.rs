use crate::{build_expression_parser, common::{ast::{Judgment, NamelessExpr, NamelessVar}, tokenizer::Token}, parser::{environment::traits::EnvironmentParser, value::ValueParserDefault, BaseParser, ExpressionParser, ParserCore, ValueParser}};

pub struct Parser {
    core: ParserCore,
}

build_expression_parser! {
    parser = Parser,
    var_type = NamelessVar,

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
        LetExprParsing,
        FunExprParsing,
        RecFunExprParsing,
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


impl ValueParser<NamelessVar> for Parser {
    fn parse_inner_expr(&self, tokens: Vec<Token>) -> Result<NamelessExpr, String>{
        let mut inner_parser = Self::new(tokens);
        inner_parser.parse_expr()
    }

    fn parse_value(&mut self) -> Result<crate::common::ast::Value<NamelessVar>, String> {
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
        let env = <Self as EnvironmentParser<NamelessVar>>::parse_env_list(self)?;
        <Self as BaseParser>::core(self).expect(Token::Turnstile)?;
        let expr = self.parse_expr()?;
        if let Some(Token::Evalto) = <Self as BaseParser>::core(self).peek() {
            while <Self as BaseParser>::core(self).peek().is_some() && <Self as BaseParser>::core(self).peek() != Some(&Token::EOF) {
                <Self as BaseParser>::core(self).advance();
            }
        }
        Ok(Judgment::NamelessEvaluation(env, expr))
    }
}
