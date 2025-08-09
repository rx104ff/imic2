use crate::{common::{ast::{core::{DBIndex, FromBool, FromGroup, FromInt, FromNil, FromUnaryOp, FromVar, NamedVar, NamelessVar, Op, Variable}, r#type::{Type}}, tokenizer::Token}, parser::{delegate::{ParseTarget, ParserDelegate}, primitive::states::{HasState, TypeVarState}, BaseParser}};

/// Primitive Parsing Traits
pub trait IntParsing<Output: FromInt> : BaseParser {
    fn check(token: &Token) -> bool { matches!(token, Token::Int(_)) }

    fn parse(&mut self) -> Result<Output, String> {
        if let Some(Token::Int(n)) = self.core().peek().cloned() {
            self.core().advance();
            // This now depends only on the FromInt trait.
            Ok(Output::from_int(n))
        } else {
            Err(format!("Expected int, but got {:?}", self.core().peek()))
        }
    }
}

pub trait BoolParsing<Output: FromBool> : BaseParser {
    fn check(token: &Token) -> bool { matches!(token, Token::Bool(_)) }

    fn parse(&mut self) -> Result<Output, String> {
        if let Some(Token::Bool(b)) = self.core().peek().cloned() {
            self.core().advance();
            Ok(Output::from_bool(b))
        } else {
            Err(format!("Expected bool, but got {:?}", self.core().peek()))
        }
    }
}

/// A trait for parsing an `int` type keyword.
pub trait IntTypeParsing<Output>: BaseParser {
    fn check(token: &Token) -> bool {
        matches!(token, Token::TypeInt)
    }
    fn parse(&mut self) -> Result<Type<Self::V>, String> {
        self.core().advance();
        Ok(Type::Int)
    }
}

/// A trait for parsing a `bool` type keyword.
pub trait BoolTypeParsing<Output>: BaseParser {
    fn check(token: &Token) -> bool {
        matches!(token, Token::TypeBool)
    }
    fn parse(&mut self) -> Result<Type<Self::V>, String> {
        self.core().advance();
        Ok(Type::Bool)
    }
}

pub trait NilParsing<Output: FromNil> : BaseParser {
    fn check(token: &Token) -> bool { matches!(token, Token::Nil) }
    
    fn parse(&mut self) -> Result<Output, String> {
        if let Some(Token::Nil) = self.core().peek().cloned() {
            self.core().advance();
            Ok(Output::from_nil())
        } else {
            Err(format!("Expected nil, but got {:?}", self.core().peek()))
        }
    }
}

pub trait VariableParsing<Output>: BaseParser 
where Self::V: ParseableVariable,
Output: FromVar<Self::V>, {
    fn check(token: &Token) -> bool
    { 
        Self::V::from_token(token).is_some()
    }

    fn parse(&mut self) -> Result<Output, String>
    where
        Self::V: ParseableVariable,
    {
        let token = self.core().peek().ok_or_else(|| "Unexpected end of input while parsing variable".to_string())?;

        // Use the helper trait to try to create the variable.
        if let Some(var) = Self::V::from_token(token) {
            self.core().advance(); // Success! Consume the token.
            Ok(Output::from_var(var))
        } else {
            Err(format!("Token {:?} cannot be parsed as a variable.", token))
        }
    }
}

pub trait ParseableVariable: Sized {
    fn check(token: &Token) -> bool;
    fn from_token(token: &Token) -> Option<Self>;
}

impl ParseableVariable for NamedVar {
    fn check(token: &Token) -> bool {
        if let Token::Ident(_) = token {
            true
        } else {
            false
        }
    }

    fn from_token(token: &Token) -> Option<Self> {
        if let Token::Ident(name) = token {
            Some(NamedVar(name.clone()))
        } else {
            None
        }
    }
}

impl ParseableVariable for NamelessVar {
    fn check(token: &Token) -> bool {
        match token {
            Token::HashVar(_) => true,
            Token::Dot => true,
            _ => false,
        }
    }
    fn from_token(token: &Token) -> Option<Self> {
        match token {
            Token::HashVar(n) => Some(NamelessVar(DBIndex(*n as usize))),
            Token::Dot => Some(NamelessVar(DBIndex(0))),
            _ => None,
        }
    }
}

pub trait UnaryMinusParsing<Output: FromUnaryOp>: ParserDelegate
where
    Self: ParseTarget<Output>,
{
    fn check(token: &Token) -> bool {
        matches!(token, Token::Minus)
    }

    fn parse(&mut self) -> Result<Output, String> {
        self.core().advance();

        let operand: Output = self.parse_inner_atom()?;

        Output::from_unary_op(Op::Sub, operand)
    }
}

pub trait GroupParsing<Output: FromGroup>: ParserDelegate
where
    Self: ParseTarget<Output>,
{
    fn check(token: &Token) -> bool {
        matches!(token, Token::LParen)
    }

    fn parse(&mut self) -> Result<Output, String> {
        self.core().advance();

        let inner_item: Output = self.parse_inner()?;

        self.core().expect(Token::RParen)?;

        Ok(Output::from_group(inner_item))
    }
}

pub trait TypeVarParsing<V>: BaseParser<V = V>
where
    Self: HasState<TypeVarState>,
    V: Variable,
{
    fn check(token: &Token) -> bool {
        matches!(token, Token::TypeVar(_))
    }

    fn parse(&mut self) -> Result<Type<V>, String> {
        if let Some(Token::TypeVar(name)) = self.core().peek().cloned() {
            self.core().advance();
            let tv = self.state_mut().resolve_var(&name);
            Ok(Type::Var(tv))
        } else {
            Err("Expected a type variable.".to_string())
        }
    }
}
