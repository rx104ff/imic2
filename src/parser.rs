// src/parser.rs

use crate::ast::{Expr, Op, Var, Value, Env};
use crate::token::Token;
use std::rc::Rc;

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

fn mark_expr_paren(expr: Expr) -> Expr {
    match expr {
        Expr::App(f, arg, _) => Expr::App(f, arg, true),
        Expr::BinOp(lhs, op, rhs, _) => Expr::BinOp(lhs, op, rhs, true),
        other => other,
    }
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
        self.parse_list_value()
    }

    fn parse_list_value(&mut self) -> Value {
        let left = self.parse_single_value();
        self.parse_list_tail(left)
    }

    fn parse_list_tail(&mut self, left: Value) -> Value {
        if let Some(Token::ColonColon) = self.peek() {
            self.advance();
            let right = self.parse_list_value();
            Value::Cons(Box::new(left), Box::new(right))
        } else {
            left
        }
    }

    fn parse_single_value(&mut self) -> Value {
        match self.peek().cloned() {
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
                // Possible function env or something else
                self.advance();
                let saved_pos = self.pos;
                let env = self.parse_env_list();
                if self.peek() == Some(&Token::RParen) {
                    self.advance();
                    if self.peek() == Some(&Token::LBracket) {
                        self.advance();
                        match self.peek() {
                            Some(Token::Fun) => return self.parse_func_val(env),
                            Some(Token::Rec) => return self.parse_rec_func_val(env),
                            _ => panic!("Expected fun or rec after ["),
                        }
                    }
                }
                // If we didn’t match the expected func env structure, it's not function
                self.pos = saved_pos - 1; // rewind to before '('
                panic!("Invalid value syntax: expected function environment or literal value.")
            }
            Some(Token::LBracket) => {
                self.advance();
                self.expect(&Token::RBracket);
                Value::Nil
            }
            _ => panic!("Invalid value syntax: {:?}", self.peek()),
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
        self.parse_cons()
    }

    fn parse_cons(&mut self) -> Expr {
        self.parse_cons_prec(false)
    }

    fn parse_cons_prec(&mut self, paren: bool) -> Expr {
        let mut expr = self.parse_add_sub_prec(false);
        while let Some(Token::ColonColon) = self.peek() {
            self.advance();
            let right = self.parse_add_sub_prec(false);
            expr = Expr::BinOp(Box::new(expr), Op::Cons, Box::new(right), paren);
        }
        expr
    }

    fn parse_add_sub(&mut self) -> Expr {
        self.parse_add_sub_prec(false)
    }

    fn parse_add_sub_prec(&mut self, paren: bool) -> Expr {
        let mut expr = self.parse_mul_div_prec(false);
        while let Some(op_token) = self.peek() {
            let op = match op_token {
                Token::Plus => Op::Add,
                Token::Minus => Op::Sub,
                _ => break,
            };
            self.advance();
            let right = self.parse_mul_div_prec(false);
            expr = Expr::BinOp(Box::new(expr), op, Box::new(right), paren);
        }
        expr
    }

    fn parse_mul_div(&mut self) -> Expr {
        self.parse_mul_div_prec(false)
    }

    fn parse_mul_div_prec(&mut self, paren: bool) -> Expr {
        let mut expr = self.parse_app_or_atom();
        while let Some(op_token) = self.peek() {
            let op = match op_token {
                Token::Star => Op::Mul,
                _ => break,
            };
            self.advance();
            let right = self.parse_app_or_atom();
            expr = Expr::BinOp(Box::new(expr), op, Box::new(right), paren);
        }
        expr
    }

    fn parse_paren_expr(&mut self) -> Expr {
        self.advance(); // consume '('
        let mut expr = self.parse_expr();
        expr = mark_expr_paren(expr);
        self.expect(&Token::RParen);

        // Handle function application after paren: (f) x
        while matches!(self.peek(), Some(Token::Ident(_)) | Some(Token::Int(_)) | Some(Token::LParen)) {
            let arg = self.parse_atom();
            expr = Expr::App(Box::new(expr), Box::new(arg), true);
        }

        expr
    }

    fn parse_app_or_atom(&mut self) -> Expr {
        let mut expr = self.parse_atom();
        while matches!(self.peek(), Some(Token::Ident(_)) | Some(Token::Int(_)) | Some(Token::LParen)) {
            let arg = self.parse_atom();
            expr = Expr::App(Box::new(expr), Box::new(arg), false);
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
                // Mark this BinOp as parenthesized if applicable
                match expr {
                    Expr::BinOp(lhs, op, rhs, _) => Expr::BinOp(lhs, op, rhs, true),
                    _ => expr
                }
            }
            Some(Token::Fun) => self.parse_fun_expr(),
            Some(Token::LBracket) => {
                self.advance();
                self.expect(&Token::RBracket);
                Expr::Nil
            },
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
