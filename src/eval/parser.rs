// src/parser.rs

use crate::eval::ast::{Expr, Op, Var, Value, Env};
use crate::eval::token::Token;
use std::rc::Rc;

fn mark_expr_paren(expr: Expr) -> Expr {
    match expr {
        Expr::BinOp(e1, op, e2, _) => Expr::BinOp(e1, op, e2, true),
        Expr::App(f, arg, _) => Expr::App(f, arg, true),
        Expr::Fun(param, body, _) => Expr::Fun(param, body, true),
        Expr::Let(v, e1, e2, _) => Expr::Let(v, e1, e2, true),
        Expr::LetRec(f, x, body, e2, _) => Expr::LetRec(f, x, body, e2, true),
        Expr::If(c, t, e, _) => Expr::If(c, t, e, true),
        Expr::Match(e, nil_case, hd, tl, cons_case, _) => Expr::Match(e, nil_case, hd, tl, cons_case, true),
        Expr::Cons(h, t, _) => Expr::Cons(h, t, true),
        other => other,
    }
}

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
        if let Some(Token::Evalto) = self.peek() {
            while self.peek().is_some() && self.peek() != Some(&Token::EOF) {
                self.advance();
            }
        }
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
    self.parse_list_value(false)
}

fn parse_list_value(&mut self, paren: bool) -> Value {
    let left = self.parse_single_value();
    self.parse_list_tail(left, paren)
}

fn parse_list_tail(&mut self, left: Value, paren: bool) -> Value {
    if let Some(Token::ColonColon) = self.peek() {
        self.advance();
        let right = self.parse_list_value(false);
        Value::Cons(Box::new(left), Box::new(right), paren)
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
            Some(Token::Bool(b)) => {
                self.advance();
                Value::Bool(b)
            }
            Some(Token::LParen) => {
                self.advance();
                let saved_pos = self.pos;
                let env = self.parse_env_list();

                // Try to parse as a function environment
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

                // Not a function env, so rewind and try to parse as a value in parens
                self.pos = saved_pos;
                let inner = self.parse_value();
                self.expect(&Token::RParen);

                match inner {
                    Value::Cons(l, r, _) => Value::Cons(l, r, true),
                    _ => inner,
                }
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
        Value::FunVal(param, Box::new(body), env, false)
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
        Value::RecFunVal(name, param, Box::new(body), env, false)
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_lt()
    }

    fn parse_lt(&mut self) -> Expr {
        let mut expr = self.parse_cons(); // parse lhs first
        while let Some(Token::Lt) = self.peek() {
            self.advance();
            let right = self.parse_cons();
            expr = Expr::BinOp(Box::new(expr), Op::Lt, Box::new(right), false);
        }
        expr
    }

    fn parse_cons(&mut self) -> Expr {
        self.parse_cons_prec(false)
    }

    fn parse_cons_prec(&mut self, paren: bool) -> Expr {
        let lhs = self.parse_add_sub_prec(false);
        self.parse_cons_tail(lhs, paren)
    }

    fn parse_cons_tail(&mut self, lhs: Expr, paren: bool) -> Expr {
        if let Some(Token::ColonColon) = self.peek() {
            self.advance();
            let rhs = self.parse_cons_prec(false);
            Expr::BinOp(Box::new(lhs), Op::Cons, Box::new(rhs), paren)
        } else {
            lhs
        }
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

    fn parse_app_or_atom(&mut self) -> Expr {
        let mut expr = self.parse_atom();
        while matches!(self.peek(), Some(Token::Ident(_)) | Some(Token::Int(_)) | Some(Token::LParen) | Some(Token::Nil)) {
            let arg = self.parse_atom();
            expr = Expr::App(Box::new(expr), Box::new(arg), false);
        }
        expr
    }

    fn parse_atom(&mut self) -> Expr {
        match self.peek() {
            Some(Token::Minus) => {
                self.advance();
                match self.peek().cloned() {
                    Some(Token::Int(n)) => {
                        self.advance();
                        Expr::Int(-(n))
                    }
                    _ => {
                        // Parse as unary minus: `-expr` => `0 - expr`
                        let expr = self.parse_atom();
                        Expr::BinOp(Box::new(Expr::Int(0)), Op::Sub, Box::new(expr), false)
                    }
                }
            }
            Some(Token::If) => self.parse_if(),
            Some(Token::Let) => self.parse_let(),
            Some(Token::Ident(_)) => Expr::Var(self.next_var()),
            Some(Token::Int(_)) => self.parse_int(),
            Some(Token::LParen) => {
                self.advance();
                let expr = self.parse_expr();
                self.expect(&Token::RParen);
                mark_expr_paren(expr)
            }
            Some(Token::Fun) => self.parse_fun_expr(),
            Some(Token::Nil) => {
                self.advance();
                Expr::Nil
            },
            Some(Token::Match) => self.parse_match(),
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
        Expr::If(Box::new(cond), Box::new(then_branch), Box::new(else_branch), false)
    }

    fn parse_let(&mut self) -> Expr {
        self.expect(&Token::Let);
        if self.peek() == Some(&Token::Rec) {
            self.advance(); // consume 'rec'
            let name    = self.next_var(); // 'name'
            self.expect(&Token::Equals); // '='
            self.expect(&Token::Fun); // 'fun'
            let param = self.next_var(); // 'n'
            self.expect(&Token::Arrow); // '->'
            let body = self.parse_expr(); // parse the function body
            self.expect(&Token::In); // 'in'
            let cont = self.parse_expr(); // continuation expression
            Expr::LetRec(name, param, Box::new(body), Box::new(cont), false)
        } else {
            let name = self.next_var();
            self.expect(&Token::Equals);
            let bound_expr = self.parse_expr();
            self.expect(&Token::In);
            let cont = self.parse_expr();
            Expr::Let(name, Box::new(bound_expr), Box::new(cont), false)
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
        Expr::Fun(param, Box::new(body), false)
    }

    fn parse_match(&mut self) -> Expr {
        self.expect(&Token::Match);
        let expr = self.parse_expr();
        self.expect(&Token::With);

        // Case 1: [] -> e2
        self.expect(&Token::Nil);
        self.expect(&Token::Arrow);
        let nil_case = self.parse_expr();

        // Case 2: | x :: y -> e3
        self.expect(&Token::Bar);
        let hd = self.next_var();
        self.expect(&Token::ColonColon);
        let tl = self.next_var();
        self.expect(&Token::Arrow);
        let cons_case = self.parse_expr();

        Expr::Match(Box::new(expr), Box::new(nil_case), hd, tl, Box::new(cons_case), false)
    }


}
