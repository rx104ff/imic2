use crate::{common::{ast::{core::{NamedVar, NamelessVar, Variable}, expr::Expr, r#type::Type, value::Value}, tokenizer::Token}, parser::{primitive::traits::{ParseableVariable, VariableParsing}, BaseParser, TypeParser, ValueParser}};

pub trait ParseMeta {
    fn separator() -> Option<Token> {
        None
    }
}

impl<V: Variable> ParseMeta for Value<V> {
    fn separator() -> Option<Token> {
        Some(Token::Equals)
    }
}

impl<V: Variable> ParseMeta for Type<V> {
    fn separator() -> Option<Token> {
        Some(Token::Colon)
    }
}

pub trait ItemParser<V: Variable, T> {
    fn parse_item(&mut self) -> Result<T, String>;
}

impl<P, V> ItemParser<V, Value<V>> for P
where
    P: ValueParser<V> + ?Sized,
    V: Variable,
{
    fn parse_item(&mut self) -> Result<Value<V>, String> {
        self.parse_value()
    }
}

impl<P, V> ItemParser<V, Type<V>> for P
where
    P: TypeParser<V> + ?Sized,
    V: Variable,
{
    fn parse_item(&mut self) -> Result<Type<V>, String> {
        self.parse_type()
    }
}

pub trait EnvironmentParser<V, T>: BaseParser<V = V>
where V: Variable{
    fn parse_env_list(&mut self) -> Result<Vec<(V, T)>, String>;
}

impl<P, V, T> EnvironmentParser<V, T> for P
where
    P: BaseParser<V = V> + ItemParser<V, T> + VariableParsing<Expr<V>> + ?Sized,
    V: Variable + Clone + ParseableVariable + EnvironmentParsingStrategy<P::V, T>,
{
    fn parse_env_list(&mut self) -> Result<Vec<(V, T)>, String> {
        V::parse_env(self)
    }
}

/// The strategy trait must also be generic over the item type `T`.
pub trait EnvironmentParsingStrategy<V: Variable + ParseableVariable, T> {
    fn parse_env<P>(parser: &mut P) -> Result<Vec<(V, T)>, String>
    where
        P: BaseParser<V = V> + ItemParser<V, T> + VariableParsing<Expr<V>> + ?Sized;
}

/// Implementation for NamedVar.
/// It no longer calls `parse_value` directly. Instead, it uses the generic `parse_item`.
impl<T: ParseMeta> EnvironmentParsingStrategy<NamedVar, T> for NamedVar {
    fn parse_env<P>(parser: &mut P) -> Result<Vec<(NamedVar, T)>, String>
    where
        P: BaseParser<V = NamedVar> + ItemParser<NamedVar, T> + VariableParsing<Expr<NamedVar>> + ?Sized,
    {
        let mut bindings = vec![];
        if parser.core().peek() == Some(&Token::Turnstile) {
            return Ok(bindings);
        }

        let separator = T::separator().ok_or("Separator token not defined for this item type.")?;

        loop {
            let var = parser.parse()?.into_variable().ok_or("Expected a variable name.")?;
            parser.core().expect(separator.clone())?;
            
            // **The crucial change**: calls the generic `parse_item` method.
            let item = parser.parse_item()?;
            bindings.push((var, item));

            if parser.core().peek() == Some(&Token::Comma) {
                parser.core().advance();
            } else {
                break;
            }
        }
        Ok(bindings)
    }
}

impl<T> EnvironmentParsingStrategy<NamelessVar, T> for NamelessVar {
    fn parse_env<P>(parser: &mut P) -> Result<Vec<(NamelessVar, T)>, String>
    where
        P: BaseParser<V = NamelessVar> + ItemParser<NamelessVar, T> + VariableParsing<Expr<NamelessVar>> + ?Sized,
    {
        let mut items = vec![];
        if parser.core().peek() == Some(&Token::Turnstile) {
            return Ok(vec![]);
        }

        loop {
            // **The crucial change**: calls the generic `parse_item` method.
            let item = parser.parse_item()?;
            items.push(item);

            if parser.core().peek() == Some(&Token::Comma) {
                parser.core().advance();
            } else {
                break;
            }
        }

        let bindings = items
            .into_iter()
            .filter_map(|item| NamelessVar::from_token(&Token::Dot).map(|binder| (binder, item)))
            .collect();
        Ok(bindings)
    }
}
