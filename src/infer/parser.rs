use crate::infer::ast::{Expr, Var, Op, Type, TypeEnv, Judgment};
use crate::infer::token::Token;

/// A helper function to mark an expression as having been parsed inside parentheses.
/// This sets the `is_paren` flag to true for any compound expression type.
fn mark_expr_paren(expr: Expr) -> Expr {
    match expr {
        Expr::Let(v, e1, e2, _) => Expr::Let(v, e1, e2, true),
        Expr::LetRec(f, p, e1, e2, _) => Expr::LetRec(f, p, e1, e2, true),
        Expr::Fun(p, b, _) => Expr::Fun(p, b, true),
        Expr::App(e1, e2, _) => Expr::App(e1, e2, true),
        Expr::If(c, t, e, _) => Expr::If(c, t, e, true),
        Expr::BinOp(e1, op, e2, _) => Expr::BinOp(e1, op, e2, true),
        Expr::Match(e, n, h, t, c, _) => Expr::Match(e, n, h, t, c, true),
        // Atomic expressions like Int, Bool, Var, and Nil don't have a paren flag.
        other => other,
    }
}

/// A recursive descent parser for the TypingML4 language.
pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    /// The main entry point for the parser.
    /// It parses a judgment of the form `env |- expr : type`
    /// and returns the parsed Judgment struct.
    pub fn parse_judgment(&mut self) -> Result<Judgment, String> {
        let env = self.parse_type_env()?;
        self.expect(Token::Turnstile)?;
        let expr = self.parse_expr()?;
        self.expect(Token::Colon)?;
        let ty = self.parse_type()?;
        Ok(Judgment { env, expr, ty })
    }

    // --- Helper Methods ---
    fn peek(&self) -> Option<&Token> { self.tokens.get(self.pos) }
    fn advance(&mut self) { self.pos += 1; }
    fn expect(&mut self, expected: Token) -> Result<(), String> {
        match self.peek() {
            Some(token) if *token == expected => { self.advance(); Ok(()) }
            Some(token) => Err(format!("Expected token {:?}, found {:?}", expected, token)),
            None => Err(format!("Expected token {:?}, but found end of input.", expected)),
        }
    }
    fn next_var(&mut self) -> Result<Var, String> {
        match self.peek().cloned() {
            Some(Token::Ident(name)) => { self.advance(); Ok(Var(name)) }
            _ => Err("Expected an identifier.".to_string()),
        }
    }

    // --- Type Environment and Type Parsing ---
    fn parse_type_env(&mut self) -> Result<TypeEnv, String> {
        let mut env = TypeEnv::new();
        if self.peek() == Some(&Token::Turnstile) { return Ok(env); }
        loop {
            let var = self.next_var()?;
            self.expect(Token::Colon)?;
            let ty = self.parse_type()?;
            env.push((var, ty));
            if self.peek() == Some(&Token::Comma) { self.advance(); } 
            else { break; }
        }
        Ok(env)
    }

    fn parse_type(&mut self) -> Result<Type, String> {
        let mut ty = self.parse_single_type()?;
        if self.peek() == Some(&Token::Arrow) {
            self.advance();
            let return_ty = self.parse_type()?;
            ty = Type::Fun(Box::new(ty), Box::new(return_ty));
        }
        Ok(ty)
    }

    fn parse_single_type(&mut self) -> Result<Type, String> {
        let ty = match self.peek().cloned() {
            Some(Token::Ident(name)) => {
                self.advance();
                match name.as_str() {
                    "int" => Type::Int,
                    "bool" => Type::Bool,
                    _ => return Err(format!("Unknown type name '{}'", name)),
                }
            }
            Some(Token::LParen) => {
                self.advance();
                let inner_ty = self.parse_type()?;
                self.expect(Token::RParen)?;
                inner_ty
            }
            _ => return Err("Expected a type name or a parenthesized type.".to_string()),
        };
        if let Some(Token::Ident(s)) = self.peek() {
            if s == "list" {
                self.advance();
                return Ok(Type::List(Box::new(ty)));
            }
        }
        Ok(ty)
    }

    // --- Full Expression Parsing Logic ---

    /// The main entrypoint for parsing an expression.
    /// It correctly dispatches to low-precedence forms first.
    fn parse_expr(&mut self) -> Result<Expr, String> {
        match self.peek() {
            Some(Token::Let) => self.parse_let_expr(),
            Some(Token::If) => self.parse_if_expr(),
            Some(Token::Fun) => self.parse_fun_expr(),
            Some(Token::Match) => self.parse_match_expr(),
            _ => self.parse_lt(), // Default to operator expressions
        }
    }
    
    fn parse_let_expr(&mut self) -> Result<Expr, String> {
        self.advance(); // consume 'let'
        if self.peek() == Some(&Token::Rec) {
            self.advance(); // consume 'rec'
            let func_name = self.next_var()?;
            self.expect(Token::Equals)?;
            let body = self.parse_expr()?;
            self.expect(Token::In)?;
            let cont = self.parse_expr()?;
            if let Expr::Fun(param, fun_body, _) = body {
                 Ok(Expr::LetRec(func_name, param, fun_body, Box::new(cont), false))
            } else {
                Err("Expected a function definition after 'let rec ='".to_string())
            }
        } else {
            let var = self.next_var()?;
            self.expect(Token::Equals)?;
            let bound_expr = self.parse_expr()?;
            self.expect(Token::In)?;
            let cont = self.parse_expr()?;
            Ok(Expr::Let(var, Box::new(bound_expr), Box::new(cont), false))
        }
    }


    fn parse_if_expr(&mut self) -> Result<Expr, String> {
        self.advance(); // consume 'if'
        let cond = self.parse_expr()?;
        self.expect(Token::Then)?;
        let then_branch = self.parse_expr()?;
        self.expect(Token::Else)?;
        let else_branch = self.parse_expr()?;
        Ok(Expr::If(Box::new(cond), Box::new(then_branch), Box::new(else_branch), false))
    }

    fn parse_fun_expr(&mut self) -> Result<Expr, String> {
        self.advance(); // consume 'fun'
        let param = self.next_var()?;
        self.expect(Token::Arrow)?;
        let body = self.parse_expr()?;
        Ok(Expr::Fun(param, Box::new(body), false))
    }

    fn parse_match_expr(&mut self) -> Result<Expr, String> {
        self.advance(); // consume 'match'
        let expr_to_match = self.parse_expr()?;
        self.expect(Token::With)?;
        self.expect(Token::Nil)?;
        self.expect(Token::Arrow)?;
        let nil_case = self.parse_expr()?;
        self.expect(Token::Bar)?;
        let head_var = self.next_var()?;
        self.expect(Token::ColonColon)?;
        let tail_var = self.next_var()?;
        self.expect(Token::Arrow)?;
        let cons_case = self.parse_expr()?;
        Ok(Expr::Match(Box::new(expr_to_match), Box::new(nil_case), head_var, tail_var, Box::new(cons_case), false))
    }

    // --- Operator Precedence Parsing ---

    fn parse_lt(&mut self) -> Result<Expr, String> {
        let mut lhs = self.parse_cons()?;
        while self.peek() == Some(&Token::Lt) {
            self.advance();
            let rhs = self.parse_cons()?;
            lhs = Expr::BinOp(Box::new(lhs), Op::Lt, Box::new(rhs), false);
        }
        Ok(lhs)
    }
    
    fn parse_cons(&mut self) -> Result<Expr, String> {
        let mut lhs = self.parse_add_sub()?;
        if self.peek() == Some(&Token::ColonColon) {
            self.advance();
            let rhs = self.parse_cons()?;
            lhs = Expr::BinOp(Box::new(lhs), Op::Cons, Box::new(rhs), false);
        }
        Ok(lhs)
    }

    fn parse_add_sub(&mut self) -> Result<Expr, String> {
        let mut lhs = self.parse_mul()?;
        while let Some(op_token) = self.peek() {
            let op = match op_token {
                Token::Plus => Op::Add, Token::Minus => Op::Sub, _ => break,
            };
            self.advance();
            let rhs = self.parse_mul()?;
            lhs = Expr::BinOp(Box::new(lhs), op, Box::new(rhs), false);
        }
        Ok(lhs)
    }

    fn parse_mul(&mut self) -> Result<Expr, String> {
        let mut lhs = self.parse_app()?;
        while self.peek() == Some(&Token::Star) {
            self.advance();
            let rhs = self.parse_app()?;
            lhs = Expr::BinOp(Box::new(lhs), Op::Mul, Box::new(rhs), false);
        }
        Ok(lhs)
    }

    fn parse_app(&mut self) -> Result<Expr, String> {
        let mut lhs = self.parse_atom()?;
        while let Some(tok) = self.peek() {
            match tok {
                Token::Int(_) | Token::Bool(_) | Token::Ident(_) | Token::LParen | Token::Nil => {
                    let rhs = self.parse_atom()?;
                    lhs = Expr::App(Box::new(lhs), Box::new(rhs), false);
                },
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn parse_atom(&mut self) -> Result<Expr, String> {
        match self.peek().cloned() {
            Some(Token::Int(n)) => { self.advance(); Ok(Expr::Int(n)) }
            Some(Token::Bool(b)) => { self.advance(); Ok(Expr::Bool(b)) }
            Some(Token::Ident(name)) => { self.advance(); Ok(Expr::Var(Var(name))) }
            Some(Token::Nil) => { self.advance(); Ok(Expr::Nil) }
            Some(Token::LParen) => {
                self.advance();
                let expr = self.parse_expr()?;
                self.expect(Token::RParen)?;
                Ok(mark_expr_paren(expr))
            }
            _ => Err("Unexpected token found at atomic level.".to_string())
        }
    }
}
