use crate::common::ast::{Type, MonoTypeEnv, Judgment};
use crate::common::parser::{ExpressionParser, ParserCore, TypeParser};
use crate::common::tokenizer::Token;

/// A recursive descent parser for the TypingML4 language.
pub struct Parser {
    core: ParserCore,
}

impl ExpressionParser for Parser {
    fn core(&mut self) -> &mut ParserCore {
        &mut self.core
    }
}

impl TypeParser for Parser {
    fn core(&mut self) -> &mut ParserCore {
        &mut self.core
    }

    fn parse_single_type(&mut self) -> Result<Type, String> {
        let ty = match self.core.peek().cloned() {
            Some(Token::Ident(name)) => {
                self.core.advance();
                match name.as_str() {
                    "int" => Type::Int,
                    "bool" => Type::Bool,
                    _ => return Err(format!("Unknown type name '{}'", name)),
                }
            }
            Some(Token::LParen) => {
                self.core.advance();
                let inner_ty = self.parse_type()?;
                self.core.expect(Token::RParen)?;
                inner_ty
            }
            _ => return Err("Expected a type name or a parenthesized type.".to_string()),
        };
        if let Some(Token::Ident(s)) = self.core.peek() {
            if s == "list" {
                self.core.advance();
                return Ok(Type::List(Box::new(ty)));
            }
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
            let var = self.core.next_var()?;
            self.core.expect(Token::Colon)?;
            let ty = self.parse_type()?;
            env.push((var, ty));
            if self.core.peek() == Some(&Token::Comma) { self.core.advance(); } 
            else { break; }
        }
        Ok(env)
    }
}
