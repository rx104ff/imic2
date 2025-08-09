use crate::{common::{ast::{core::{NamedVar, NamelessVar, Variable}, env::Env, expr::Expr, r#type::{Scheme, Type}, value::Value}, tokenizer::Token}, parser::{primitive::{traits::{ParseableVariable, VariableParsing}, HasState, TypeVarState}, BaseParser, TypeParser, ValueParser}};

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

impl<V: Variable> ParseMeta for Scheme<V> {
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

impl<P, V> ItemParser<V, Scheme<V>> for P
where
    P: TypeParser<V> + BaseParser<V = V> + HasState<TypeVarState> + ?Sized,
    V: Variable,
{
    fn parse_item(&mut self) -> Result<Scheme<V>, String> {
        let initial_pos = self.core().pos();
        let mut potential_var_names = vec![];
        while let Some(Token::TypeVar(name)) = self.core().peek().cloned() {
            potential_var_names.push(name);
            self.core().advance();
        }

        if !potential_var_names.is_empty() && self.core().peek() == Some(&Token::Dot) {
            self.core().advance();
            let mut quantified_vars = vec![];
            for name in potential_var_names {
                let tv = self.state_mut().resolve_var(&name);
                quantified_vars.push(tv);
            }
            let ty = self.parse_type()?;
            Ok(Scheme(Type::Scheme(Box::new(quantified_vars), Box::new(ty))))
        } else {
            self.core().initial(initial_pos);
            Ok(Scheme(self.parse_type()?))
        }
    }
}

pub trait SchemeParser<V: Variable>: TypeParser<V> {
    fn parse_scheme_or_mono_type(&mut self) -> Result<Type<V>, String>;
}

impl<P, V> SchemeParser<V> for P
where
    P: TypeParser<V> + BaseParser<V = V> + HasState<TypeVarState> + ?Sized,
    V: Variable,
{
    fn parse_scheme_or_mono_type(&mut self) -> Result<Type<V>, String> {
        let initial_pos = self.core().pos();
        let mut potential_var_names = vec![];
        while let Some(Token::TypeVar(name)) = self.core().peek().cloned() {
            potential_var_names.push(name);
            self.core().advance();
        }

        if !potential_var_names.is_empty() && self.core().peek() == Some(&Token::Dot) {
            self.core().advance(); // Consume '.'
            let mut quantified_vars = vec![];
            for name in potential_var_names {
                let tv = self.state_mut().resolve_var(&name);
                quantified_vars.push(tv);
            }
            let ty = self.parse_type()?;
            Ok(Type::Scheme(Box::new(quantified_vars), Box::new(ty)))
        } else {
            self.core().initial(initial_pos);
            self.parse_type()
        }
    }
}


pub trait EnvironmentParser<V, T>: BaseParser<V = V>
where V: Variable{
    fn parse_env_list(&mut self) -> Result<Env<V, T>, String>;
}

impl<P, V, T> EnvironmentParser<V, T> for P
where
    P: BaseParser<V = V> + ItemParser<V, T> + VariableParsing<Expr<V>> + ?Sized,
    V: Variable + Clone + ParseableVariable + EnvironmentParsingStrategy<P::V, T>,
{
    fn parse_env_list(&mut self) -> Result<Env<V, T>, String> {
        V::parse_env(self)
    }
}

/// The strategy trait must also be generic over the item type `T`.
pub trait EnvironmentParsingStrategy<V: Variable + ParseableVariable, T> {
    fn parse_env<P>(parser: &mut P) -> Result<Env<V, T>, String>
    where
        P: BaseParser<V = V> + ItemParser<V, T> + VariableParsing<Expr<V>> + ?Sized;
}

impl<T: ParseMeta> EnvironmentParsingStrategy<NamedVar, T> for NamedVar {
    fn parse_env<P>(parser: &mut P) -> Result<Env<NamedVar, T>, String>
    where
        P: BaseParser<V = NamedVar> + ItemParser<NamedVar, T> + VariableParsing<Expr<NamedVar>> + ?Sized,
    {
        let mut bindings = Env(vec![]);
        if parser.core().peek() == Some(&Token::Turnstile) {
            return Ok(bindings);
        }

        let separator = T::separator().ok_or("Separator token not defined for this item type.")?;

        loop {
            let var = parser.parse()?.into_variable().ok_or("Expected a variable name.")?;
            parser.core().expect(separator.clone())?;
            
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
    fn parse_env<P>(parser: &mut P) -> Result<Env<NamelessVar, T>, String>
    where
        P: BaseParser<V = NamelessVar> + ItemParser<NamelessVar, T> + VariableParsing<Expr<NamelessVar>> + ?Sized,
    {
        let mut bindings = Env(vec![]);
        if parser.core().peek() == Some(&Token::Turnstile) {
            return Ok(bindings);
        }

        loop {
            let item = parser.parse_item()?;
            let binder = NamelessVar::from_token(&Token::Dot)
                .ok_or_else(|| "Internal Error: Could not create a dot binder for nameless environment parsing.".to_string())?;
            
            bindings.push((binder, item));

            if parser.core().peek() == Some(&Token::Comma) {
                parser.core().advance();
            } else {
                break;
            }
        }
        
        Ok(bindings)
    }
}
