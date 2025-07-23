use crate::{common::{ast::{Expr, Value, Variable}, tokenizer::Token}, parser::{environment::traits::EnvironmentParser, expression::traits::ExpressionParser, primitive::traits::{ParseableVariable, VariableParsing}}};


pub trait ValueParser<V>:  ExpressionParser<V>
where V: Variable {
    fn parse_inner_expr(&self, tokens: Vec<Token>) -> Result<Expr<V>, String>;
    fn parse_value(&mut self) -> Result<Value<V>, String>;
}

pub trait ValueParserDefault<V>: ValueParser<V>
where V: Variable {
    // fn parse_env_list(&mut self) -> Result<Vec<(Var, Value<E>)>, String>;
    fn parse_value(&mut self) -> Result<Value<V>, String>;
    fn parse_list_value(&mut self) -> Result<Value<V>, String>;
    fn parse_list_tail(&mut self, left: Value<V>) -> Result<Value<V>, String>;
    fn parse_value_atom(&mut self) -> Result<Value<V>, String>;
    fn collect_tokens_until_rbracket(&mut self) -> Vec<Token>;
    fn parse_func_val(&mut self, env: Vec<(V, Value<V>)>) -> Result<Value<V>, String>;
    fn parse_rec_func_val(&mut self, env: Vec<(V, Value<V>)>) -> Result<Value<V>, String>;
}

impl<P, V> ValueParserDefault<V> for P
where
    P: ValueParser<V> + EnvironmentParser<V> + VariableParsing<Expr<V>> + ?Sized,
    V: ParseableVariable + Variable
{
    fn parse_value(&mut self) -> Result<Value<V>, String> {
        self.parse_list_value()
    }

    fn parse_list_value(&mut self) -> Result<Value<V>, String> {
        let left = self.parse_value_atom()?;
        self.parse_list_tail(left)
    }

    fn parse_value_atom(&mut self) -> Result<Value<V>, String> {
        match self.core().peek().cloned() {
            Some(Token::Int(n)) => { self.core().advance(); Ok(Value::Int(n)) }
            Some(Token::Bool(b)) => { self.core().advance(); Ok(Value::Bool(b)) }
            Some(Token::LParen) => {
                self.core().advance();
                let saved_pos = self.core().pos();
                
                if let Ok(env) = <Self as EnvironmentParser<V>>::parse_env_list(self) {
                    if self.core().peek() == Some(&Token::RParen) {
                        self.core().advance();
                        if self.core().peek() == Some(&Token::LBracket) {
                            self.core().advance();
                             match self.core().peek() {
                                Some(Token::Fun) => return self.parse_func_val(env),
                                Some(Token::LetRec) => return self.parse_rec_func_val(env),
                                _ => {}
                            }
                        }
                    }
                }

                self.core().initial(saved_pos);
                let inner = self.parse_value()?;
                self.core().expect(Token::RParen)?;
                match inner {
                    Value::Cons(l, r) => Ok(Value::Cons(l, r)),
                    _ => Ok(inner),
                }
            }
            Some(Token::Nil) => { self.core().advance(); Ok(Value::Nil) }
            _ => Err(format!("Invalid value syntax: {:?}", self.core().peek())),
        }
    }

    fn parse_list_tail(&mut self, left: Value<V>) -> Result<Value<V>, String> {
        if self.core().peek() == Some(&Token::ColonColon) {
            self.core().advance();
            let right = self.parse_list_value()?;
            Ok(Value::Cons(Box::new(left), Box::new(right)))
        } else {
            Ok(left)
        }
    }

    fn collect_tokens_until_rbracket(&mut self) -> Vec<Token> {
        let mut tokens = vec![];
        let mut depth = 1;
        while let Some(token) = self.core().peek().cloned() {
            if token == Token::LBracket { depth += 1; }
            self.core().advance();
            if token == Token::RBracket {
                depth -= 1;
                if depth == 0 { break; }
            }
            tokens.push(token);
        }
        tokens
    }

    fn parse_func_val(&mut self, env: Vec<(V, Value<V>)>) -> Result<Value<V>, String> {
        self.core().expect(Token::Fun)?;
        let param = <Self as VariableParsing<Expr<V>>>::parse(self)?.into_variable().ok_or("Expected a variable name in `let` expression, but found something else.")?;
        self.core().expect(Token::Arrow)?;
        let body_tokens = self.collect_tokens_until_rbracket();
        let body = self.parse_inner_expr(body_tokens)?;
        Ok(Value::FunVal(param, Box::new(body), env))
    }

    fn parse_rec_func_val(&mut self, env: Vec<(V, Value<V>)>) -> Result<Value<V>, String> {
        self.core().expect(Token::Rec)?;
        let name = <Self as VariableParsing<Expr<V>>>::parse(self)?.into_variable().ok_or("Expected a variable name in `let` expression, but found something else.")?;
        self.core().expect(Token::Equals)?;
        self.core().expect(Token::Fun)?;
        let param = <Self as VariableParsing<Expr<V>>>::parse(self)?.into_variable().ok_or("Expected a variable name in `let` expression, but found something else.")?;
        self.core().expect(Token::Arrow)?;
        let body_tokens = self.collect_tokens_until_rbracket();
        let body = self.parse_inner_expr(body_tokens)?;
        Ok(Value::RecFunVal(name, param, Box::new(body), env))
    }
}