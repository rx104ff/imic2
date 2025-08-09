use crate::{common::{ast::{core::{Op, Variable}, expr::Expr, value::Value}, tokenizer::Token}, parser::{environment::traits::EnvironmentParser, expression::traits::ExpressionParser, primitive::traits::{ParseableVariable, VariableParsing}, BaseParser}};

pub trait ValueParser<V>: BaseParser<V = V>
where
    V: Variable,
{
    fn parse_value(&mut self) -> Result<Value<V>, String>;
    fn parse_value_atom(&mut self) -> Result<Value<V>, String>;
}

pub trait ConsValueParsing<V>: ValueParser<V>
where
    V: Variable,
{
    fn check(token: &Token) -> bool {
        matches!(token, Token::ColonColon)
    }

    fn handle(&mut self) -> Option<Op> {
        if self.core().peek() == Some(&Token::ColonColon) {
            self.core().advance();
            return Some(Op::Cons);
        }
        None
    }

    fn is_right_assoc() -> bool {
        true
    }
}

pub trait FunValParsing<V>: ValueParser<V> + ExpressionParser<V> + EnvironmentParser<V, Value<V>> + VariableParsing<Expr<V>>
where
    V: Variable + ParseableVariable,
{
    fn check(token: &Token) -> bool {
        matches!(token, Token::LParen)
    }

    fn parse(&mut self) -> Result<Value<V>, String> {
        self.core().advance(); // consume '('
        let env = <Self as EnvironmentParser<V, Value<V>>>::parse_env_list(self)?;
        self.core().expect(Token::RParen)?;
        self.core().expect(Token::LBracket)?;
        self.core().expect(Token::Fun)?;
        let param = <Self as VariableParsing<Expr<V>>>::parse(self)?
            .into_variable()
            .ok_or("Expected a variable name.")?;
        self.core().expect(Token::Arrow)?;
        let body = self.parse_expr()?;
        self.core().expect(Token::RBracket)?;
        Ok(Value::FunVal(param, Box::new(body), env))
    }
}

pub trait RecFunValParsing<V>: ValueParser<V> + ExpressionParser<V> + EnvironmentParser<V, Value<V>> + VariableParsing<Expr<V>>
where
    V: Variable + ParseableVariable,
{
    fn check(token: &Token) -> bool {
        matches!(token, Token::LParen)
    }

    fn parse(&mut self) -> Result<Value<V>, String> {
        self.core().advance(); // consume '('
        let env = <Self as EnvironmentParser<V, Value<V>>>::parse_env_list(self)?;
        self.core().expect(Token::RParen)?;
        self.core().expect(Token::LBracket)?;
        self.core().expect(Token::Rec)?;
        let name = <Self as VariableParsing<Expr<V>>>::parse(self)?
            .into_variable()
            .ok_or("Expected a variable name.")?;
        self.core().expect(Token::Equals)?;
        self.core().expect(Token::Fun)?;
        let param = <Self as VariableParsing<Expr<V>>>::parse(self)?
            .into_variable()
            .ok_or("Expected a variable name.")?;
        self.core().expect(Token::Arrow)?;
        let body = self.parse_expr()?;
        self.core().expect(Token::RBracket)?;
        Ok(Value::RecFunVal(name, param, Box::new(body), env))
    }
}
