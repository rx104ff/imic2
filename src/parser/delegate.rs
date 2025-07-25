use crate::{common::ast::{Expr, Type, Value, Variable}, parser::{BaseParser, ExpressionParser, TypeParser, ValueParser}};

pub trait ParseTarget<T>: BaseParser {
    fn parse_target(&mut self) -> Result<T, String>;
    fn parse_target_atom(&mut self) -> Result<T, String>;
}

pub trait ParserDelegate: BaseParser {
    fn parse_inner<T>(&mut self) -> Result<T, String>
    where
        Self: ParseTarget<T>,
    {
        self.parse_target()
    }

    fn parse_inner_atom<T>(&mut self) -> Result<T, String>
    where
        Self: ParseTarget<T>,
    {
        self.parse_target_atom()
    }
}

impl<P: BaseParser> ParserDelegate for P {}

impl<P, V> ParseTarget<Expr<V>> for P
where
    P: ExpressionParser<V> + ?Sized,
    V: Variable,
{
    fn parse_target(&mut self) -> Result<Expr<V>, String> {
        self.parse_expr()
    }
    fn parse_target_atom(&mut self) -> Result<Expr<V>, String> {
        self.parse_expr_atom()
    }
}

impl<P, V> ParseTarget<Value<V>> for P
where
    P: ValueParser<V> + ?Sized,
    V: Variable,
{
    fn parse_target(&mut self) -> Result<Value<V>, String> {
        self.parse_value()
    }
    fn parse_target_atom(&mut self) -> Result<Value<V>, String> {
        self.parse_value()
    }
}

impl<P, V> ParseTarget<Type<V>> for P
where
    P: TypeParser<V> + ?Sized,
    V: Variable,
{
    fn parse_target(&mut self) -> Result<Type<V>, String> {
        self.parse_type()
    }
    fn parse_target_atom(&mut self) -> Result<Type<V>, String> {
        self.parse_type_atom()
    }
}
