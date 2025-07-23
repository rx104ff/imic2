use crate::parser::primitive::traits::VariableParsing;
use crate::parser::{ParserCore, TypeParser, ExpressionParser};
use crate::{build_expression_parser};
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


impl TypeParser for Parser {
    fn core(&mut self) -> &mut ParserCore {
        &mut self.core
    }

    fn parse_single_type(&mut self) -> Result<Type, String> {
        let ty = match self.core.peek().cloned() {
            Some(Token::TypeInt) => {
                self.core.advance();
                Type::Int
            }
            Some(Token::TypeBool) => {
                self.core.advance();
                Type::Bool
            }
            Some(Token::LParen) => {
                self.core.advance();
                let inner_ty = self.parse_type()?;
                self.core.expect(Token::RParen)?;
                inner_ty
            }
            _ => return Err("Expected a type name or a parenthesized type.".to_string()),
        };
        if let Some(Token::TypeList) = self.core.peek() {
            self.core.advance();
            return Ok(Type::List(Box::new(ty)));
        }
        Ok(ty)
    }
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            core: ParserCore::new(tokens),
        }
    }

    /// The main entry point for the parser.
    /// It parses a judgment of the form `env |- expr : type`
    /// and returns the parsed Judgment struct.
    pub fn parse(&mut self) -> Result<Judgment, String> {
        let env = self.parse_type_env()?;
        self.core.expect(Token::Turnstile)?;
        let expr = self.parse_expr()?;
        self.core.expect(Token::Colon)?;
        let ty = self.parse_type()?;
        Ok(Judgment::Infer(env, expr, ty))
    }

    // --- Type Environment and Type Parsing ---
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
