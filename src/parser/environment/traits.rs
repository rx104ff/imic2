use crate::{common::{ast::{Expr, NamedVar, NamelessVar, Value, Variable}, tokenizer::Token}, parser::{primitive::traits::{ParseableVariable, VariableParsing}, BaseParser, ValueParser}};

pub trait EnvironmentParser<V>: BaseParser<V = V> 
where V: Variable{
    fn parse_env_list(&mut self) -> Result<Vec<(V, Value<V>)>, String>;
}

impl<P, V> EnvironmentParser<V> for P
where
    P: BaseParser + ValueParser<V> + VariableParsing<Expr<V>> + ?Sized,
    V: Variable + Clone + ParseableVariable + EnvironmentParsingStrategy<P::V>,
{
    fn parse_env_list(&mut self) -> Result<Vec<(V, Value<V>)>, String> {
        V::parse_env(self)
    }
}

pub trait EnvironmentParsingStrategy<V: Variable + ParseableVariable> {
    fn parse_env<P>(parser: &mut P) -> Result<Vec<(V, Value<V>)>, String>
    where
        P: BaseParser<V = V> + ValueParser<V> + VariableParsing<Expr<V>> + ?Sized;
}

impl EnvironmentParsingStrategy<NamedVar> for NamedVar {
    fn parse_env<P>(parser: &mut P) -> Result<Vec<(NamedVar, Value<NamedVar>)>, String>
    where
        P: BaseParser<V = NamedVar> + ValueParser<NamedVar> + VariableParsing<Expr<NamedVar>> + ?Sized,
    {
        let mut bindings = vec![];
        if parser.core().peek() == Some(&Token::Turnstile) {
            return Ok(bindings);
        }

        loop {
            let var = parser.parse()?.into_variable().ok_or("Expected a variable name.")?;
            parser.core().expect(Token::Equals)?;
            let val = parser.parse_value()?;
            bindings.push((var, val));

            if parser.core().peek() == Some(&Token::Comma) {
                parser.core().advance();
            } else {
                break;
            }
        }
        Ok(bindings)
    }
}

impl EnvironmentParsingStrategy<NamelessVar> for NamelessVar {
    fn parse_env<P>(parser: &mut P) -> Result<Vec<(NamelessVar, Value<NamelessVar>)>, String>
    where
        P: BaseParser<V = NamelessVar> + ValueParser<NamelessVar> + VariableParsing<Expr<NamelessVar>> + ?Sized,
    {
        let mut values = vec![];
        if parser.core().peek() == Some(&Token::Turnstile) {
            return Ok(vec![]);
        }

        loop {
            let val = parser.parse_value()?;
            values.push(val);

            if parser.core().peek() == Some(&Token::Comma) {
                parser.core().advance();
            } else {
                break;
            }
        }

        let bindings = values
            .into_iter()
            .filter_map(|val| NamelessVar::from_token(&Token::Dot).map(|binder| (binder, val)))
            .collect();
        Ok(bindings)
    }
}
