// src/parser.rs

use crate::ast::{Expr, Op, Var, Value, Env};
use crate::token::Token;
use std::rc::Rc;

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn expect(&mut self, expected: &Token) {
        let token = self.peek().cloned();
        if token.as_ref() != Some(expected) {
            panic!("Expected '{:?}', found '{:?}'", expected, token);
        }
        self.advance();
    }

    fn next_var(&mut self) -> Var {
        let token = self.peek().cloned();
        match token {
            Some(Token::Ident(name)) => {
                self.advance();
                Var::from(name)
            }
            _ => panic!("Expected identifier, found '{:?}'", token),
        }
    }

    pub fn parse_program(&mut self) -> (Env, Expr) {
        let env = if let Some(Token::Ident(_)) = self.peek() {
            self.parse_env_list()
        } else {
            Rc::new(vec![])
        };
        self.expect(&Token::Bar);
        self.expect(&Token::Minus);
        let expr = self.parse_expr();
        (env, expr)
    }

    fn parse_env_list(&mut self) -> Env {
        let mut bindings = vec![];
        loop {
            let token = self.peek().cloned();
            match token {
                Some(Token::Ident(_)) => {
                    let var = self.next_var();
                    self.expect(&Token::Equals);
                    let val = self.parse_value();
                    bindings.push((var, val));
                    if matches!(self.peek(), Some(Token::Comma)) {
                        self.advance();
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
        Rc::new(bindings)
    }

    fn parse_value(&mut self) -> Value {
        let token = self.peek().cloned();
        match token {
            Some(Token::Int(n)) => {
                self.advance();
                Value::Int(n)
            }
            Some(Token::Ident(ref name)) if name == "True" => {
                self.advance();
                Value::Bool(true)
            }
            Some(Token::Ident(ref name)) if name == "False" => {
                self.advance();
                Value::Bool(false)
            }
            Some(Token::LParen) => {
                self.advance();
                let env = self.parse_env_list();
                self.expect(&Token::RParen);
                self.expect(&Token::LBracket);
                let val = match self.peek() {
                    Some(Token::Fun) => self.parse_func_val(env),
                    Some(Token::Rec) => self.parse_rec_func_val(env),
                    _ => panic!("Expected fun or rec"),
                };
                val
            }
            _ => panic!("Invalid value syntax: {:?}", token),
        }
    }

    fn collect_tokens_until_rbracket(&mut self) -> Vec<Token> {
        let mut tokens = vec![];
        let mut depth = 1;
        while let Some(token) = self.peek().cloned() {
            self.advance();
            match token {
                Token::LBracket => {
                    depth += 1;
                }
                Token::RBracket => {
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                }
                _ => {}
            }
            tokens.push(token);
        }
        tokens
    }

    fn parse_func_val(&mut self, env: Env) -> Value {
        self.expect(&Token::Fun);
        let param = self.next_var();
        self.expect(&Token::Arrow);
        let body_tokens = self.collect_tokens_until_rbracket();
        let mut inner_parser = Parser::new(body_tokens);
        let body = inner_parser.parse_expr();
        Value::FunVal(param, Box::new(body), env)
    }

    fn parse_rec_func_val(&mut self, env: Env) -> Value {
        self.expect(&Token::Rec);
        let name: Var = self.next_var();
        self.expect(&Token::Equals);
        self.expect(&Token::Fun);
        let param = self.next_var();
        self.expect(&Token::Arrow);
        let body_tokens = self.collect_tokens_until_rbracket();
        let mut inner_parser = Parser::new(body_tokens);
        let body = inner_parser.parse_expr();
        Value::RecFunVal(name, param, Box::new(body), env)
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_add_sub()
    }

    fn parse_add_sub(&mut self) -> Expr {
        let mut expr = self.parse_mul_div();
        while let Some(op) = self.peek() {
            match op {
                Token::Plus | Token::Minus => {
                    let op = match self.peek().unwrap() {
                        Token::Plus => Op::Add,
                        Token::Minus => Op::Sub,
                        _ => unreachable!(),
                    };
                    self.advance();
                    let right = self.parse_mul_div();
                    expr = Expr::BinOp(Box::new(expr), op, Box::new(right));
                }
                _ => break,
            }
        }
        expr
    }

    fn parse_mul_div(&mut self) -> Expr {
        let mut expr = self.parse_app_or_atom();
        while let Some(op) = self.peek() {
            match op {
                Token::Star => {
                    self.advance();
                    let right = self.parse_app_or_atom();
                    expr = Expr::BinOp(Box::new(expr), Op::Mul, Box::new(right));
                }
                _ => break,
            }
        }
        expr
    }

    fn parse_app_or_atom(&mut self) -> Expr {
        let mut expr = self.parse_atom();
        while matches!(self.peek(), Some(Token::Ident(_)) | Some(Token::Int(_)) | Some(Token::LParen)) {
            let arg = self.parse_atom();
            expr = Expr::App(Box::new(expr), Box::new(arg));
        }
        expr
    }

    fn parse_atom(&mut self) -> Expr {
        match self.peek() {
            Some(Token::If) => self.parse_if(),
            Some(Token::Let) => self.parse_let(),
            Some(Token::Ident(_)) => Expr::Var(self.next_var()),
            Some(Token::Int(_)) => self.parse_int(),
            Some(Token::LParen) => {
                self.advance();
                let expr = self.parse_expr();
                self.expect(&Token::RParen);
                expr
            }
            Some(Token::Fun) => self.parse_fun_expr(),
            _ => panic!("Unexpected token in expression: {:?}", self.peek()),
        }
    }

    fn parse_if(&mut self) -> Expr {
        self.expect(&Token::If);
        let cond = self.parse_expr();
        self.expect(&Token::Then);
        let then_branch = self.parse_expr();
        self.expect(&Token::Else);
        let else_branch = self.parse_expr();
        Expr::If(Box::new(cond), Box::new(then_branch), Box::new(else_branch))
    }

    fn parse_let(&mut self) -> Expr {
        self.expect(&Token::Let);
        let name = self.next_var();
        if self.peek() == Some(&Token::Rec) {
            self.advance();
            let param = self.next_var();
            self.expect(&Token::Arrow);
            let body = self.parse_expr();
            self.expect(&Token::In);
            let cont = self.parse_expr();
            Expr::LetRec(name, param, Box::new(body), Box::new(cont))
        } else {
            self.expect(&Token::Equals);
            let bound_expr = self.parse_expr();
            self.expect(&Token::In);
            let cont = self.parse_expr();
            Expr::Let(name, Box::new(bound_expr), Box::new(cont))
        }
    }

    fn parse_int(&mut self) -> Expr {
        let token = self.peek().cloned();
        match token {
            Some(Token::Int(n)) => {
                self.advance();
                Expr::Int(n)
            }
            _ => panic!("Expected int"),
        }
    }

    fn parse_fun_expr(&mut self) -> Expr {
        self.expect(&Token::Fun);
        let param = self.next_var();
        self.expect(&Token::Arrow);
        let body = self.parse_expr();
        Expr::Fun(param, Box::new(body))
    }
}
