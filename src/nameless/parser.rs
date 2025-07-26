use crate::{build_expression_parser, build_value_parser, common::{ast::{core::NamelessVar, judgement::Judgment, value::{NamedValue, NamelessValue}}, tokenizer::Token}, parser::{environment::traits::EnvironmentParser, BaseParser, ExpressionParser, ParserCore, ValueParser}};

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

build_value_parser! {
    parser = Parser,
    var_type = NamelessVar,

    primitive_parsers: [
        IntParsing,
        BoolParsing,
        NilParsing,
        GroupParsing
    ],

    dispatch_parsers: [
        FunValParsing,
        RecFunValParsing
    ],

    binop_chain: [
        { ConsValueParsing }
    ]
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { core: ParserCore::new(tokens) }
    }
    
    /// The unique entry point for the `eval` parser.
    /// It parses a judgment of the form `Γ ⊢ e evalto v`.
    pub fn parse(&mut self) -> Result<Judgment, String> {
        let env = <Self as EnvironmentParser<NamelessVar, NamelessValue>>::parse_env_list(self)?;
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
