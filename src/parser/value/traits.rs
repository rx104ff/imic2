use crate::{common::{ast::{Expr, Value}, tokenizer::Token}, parser::expression::traits::{ExpressionParser, ParseableVariable, VariableParser}};


pub trait ValueParser:  ExpressionParser {
    fn parse_inner_expr(&self, tokens: Vec<Token>) -> Result<Expr<Self::V>, String>;
}

pub trait ValueParserDefault: ValueParser {
    // fn parse_env_list(&mut self) -> Result<Vec<(Var, Value<E>)>, String>;
    fn parse_value(&mut self) -> Result<Value<Self::V>, String>;
    fn parse_list_value(&mut self, paren: bool) -> Result<Value<Self::V>, String>;
    fn parse_list_tail(&mut self, left: Value<Self::V>, paren: bool) -> Result<Value<Self::V>, String>;
    fn parse_value_atom(&mut self) -> Result<Value<Self::V>, String>;
    fn collect_tokens_until_rbracket(&mut self) -> Vec<Token>;
    fn parse_func_val(&mut self, env: Vec<(Self::V, Value<Self::V>)>) -> Result<Value<Self::V>, String>;
    fn parse_rec_func_val(&mut self, env: Vec<(Self::V, Value<Self::V>)>) -> Result<Value<Self::V>, String>;
}

impl<P> ValueParserDefault for P
where
    P: ValueParser + VariableParser + ?Sized,
    P::V: ParseableVariable
{
    fn parse_value(&mut self) -> Result<Value<Self::V>, String> {
        self.parse_list_value(false)
    }

    fn parse_list_value(&mut self, paren: bool) -> Result<Value<Self::V>, String> {
        let left = self.parse_value_atom()?;
        self.parse_list_tail(left, paren)
    }

    fn parse_value_atom(&mut self) -> Result<Value<Self::V>, String> {
        match self.core().peek().cloned() {
            Some(Token::Int(n)) => { self.core().advance(); Ok(Value::Int(n)) }
            Some(Token::Bool(b)) => { self.core().advance(); Ok(Value::Bool(b)) }
            Some(Token::LParen) => {
                self.core().advance();
                let saved_pos = self.core().pos();
                
                if let Ok(env) = <Self as VariableParser>::parse_env_list(self) {
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
                    Value::Cons(l, r, _) => Ok(Value::Cons(l, r, true)),
                    _ => Ok(inner),
                }
            }
            Some(Token::Nil) => { self.core().advance(); Ok(Value::Nil) }
            _ => Err(format!("Invalid value syntax: {:?}", self.core().peek())),
        }
    }

    fn parse_list_tail(&mut self, left: Value<Self::V>, paren: bool) -> Result<Value<Self::V>, String> {
        if self.core().peek() == Some(&Token::ColonColon) {
            self.core().advance();
            let right = self.parse_list_value(false)?;
            Ok(Value::Cons(Box::new(left), Box::new(right), paren))
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

    fn parse_func_val(&mut self, env: Vec<(Self::V, Value<Self::V>)>) -> Result<Value<Self::V>, String> {
        self.core().expect(Token::Fun)?;
        let param = <Self as VariableParser>::parse(self)?.into_variable().ok_or("Expected a variable name in `let` expression, but found something else.")?;
        self.core().expect(Token::Arrow)?;
        let body_tokens = self.collect_tokens_until_rbracket();
        let body = self.parse_inner_expr(body_tokens)?;
        Ok(Value::FunVal(param, Box::new(body), env, false))
    }

    fn parse_rec_func_val(&mut self, env: Vec<(Self::V, Value<Self::V>)>) -> Result<Value<Self::V>, String> {
        self.core().expect(Token::Rec)?;
        let name = <Self as VariableParser>::parse(self)?.into_variable().ok_or("Expected a variable name in `let` expression, but found something else.")?;
        self.core().expect(Token::Equals)?;
        self.core().expect(Token::Fun)?;
        let param = <Self as VariableParser>::parse(self)?.into_variable().ok_or("Expected a variable name in `let` expression, but found something else.")?;
        self.core().expect(Token::Arrow)?;
        let body_tokens = self.collect_tokens_until_rbracket();
        let body = self.parse_inner_expr(body_tokens)?;
        Ok(Value::RecFunVal(name, param, Box::new(body), env, false))
    }
}