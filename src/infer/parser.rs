use crate::parser::primitive::traits::VariableParsing;
use crate::parser::{BaseParser, ExpressionParser, ParserCore, TypeParser};
use crate::{build_expression_parser, build_type_parser};
use crate::common::ast::{Expr, Judgment, MonoTypeEnv, NamedVar, Type};
use crate::common::tokenizer::Token;

/// A recursive descent parser for the TypingML4 language.
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

build_type_parser! {
    parser = Parser,
    var_type = NamedVar,

    primitive_parsers: [
        IntTypeParsing,
        BoolTypeParsing,
        GroupParsing
    ],
    postfix_parsers: [
        ListTypeParsing
    ],
    binop_chain: [
        { FunTypeParsing }
    ]
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            core: ParserCore::new(tokens),
        }
    }

    pub fn parse(&mut self) -> Result<Judgment, String> {
        let env = self.parse_type_env()?;
        self.core.expect(Token::Turnstile)?;
        let expr = self.parse_expr()?;
        self.core.expect(Token::Colon)?;
        let ty = self.parse_type()?;
        Ok(Judgment::Infer(env, expr, ty))
    }

    fn parse_type_env(&mut self) -> Result<MonoTypeEnv, String> {
        let mut env = MonoTypeEnv::new();
        if self.core.peek() == Some(&Token::Turnstile) { return Ok(env); }
        loop {

            let var = <Self as VariableParsing<Expr<NamedVar>>>::parse(self)?.into_variable().ok_or("Expected a variable name in `let` expression, but found something else.")?;
            self.core.expect(Token::Colon)?;
            let ty = self.parse_type()?;
            env.push((var, ty));
            if self.core.peek() == Some(&Token::Comma) { self.core.advance(); } 
            else { break; }
        }
        Ok(env)
    }
}
