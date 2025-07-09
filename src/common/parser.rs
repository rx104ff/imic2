use crate::common::{ast::{DBIndex, Expr, NamedExpr, Op, Type, Value, Var}, tokenizer::Token};

pub trait Mode {
    type Var: std::fmt::Display + Clone;
}

pub struct Named;
pub struct Nameless;

impl Mode for Named {
    type Var = Var;
}

impl Mode for Nameless {
    type Var = DBIndex;
}


pub struct ParserCore {
    tokens: Vec<Token>,
    pos: usize,
}

impl ParserCore {
    pub fn new(tokens: Vec<Token>) -> Self {
        ParserCore { tokens, pos: 0 }
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    pub fn initial(&mut self, initial_pos: usize) {
        self.pos = initial_pos
    }

    pub fn peek(&self) -> Option<&Token> { 
        self.tokens.get(self.pos) 
    }

    pub fn advance(&mut self) {
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
    }

    pub fn expect(&mut self, expected: Token) -> Result<(), String> {
        match self.peek() {
            Some(token) if *token == expected => { self.advance(); Ok(()) }
            Some(token) => Err(format!("Expected token {:?}, found {:?}", expected, token)),
            None => Err(format!("Expected token {:?}, but found end of input.", expected)),
        }
    }

    pub fn next_var(&mut self) -> Result<Var, String> {
        match self.peek().cloned() {
            Some(Token::Ident(name)) => {
                self.advance();
                Ok(Var(name))
            }
            _ => Err("Expected an identifier.".to_string()),
        }
    }
}

/// A helper function to mark an expression as having been parsed inside parentheses.
fn mark_expr_paren<V>(expr: Expr<V>) -> Expr<V> {
    match expr {
        Expr::Let(v, e1, e2, _) => Expr::Let(v, e1, e2, true),
        Expr::LetRec(f, p, e1, e2, _) => Expr::LetRec(f, p, e1, e2, true),
        Expr::Fun(p, b, _) => Expr::Fun(p, b, true),
        Expr::App(e1, e2, _) => Expr::App(e1, e2, true),
        Expr::If(c, t, e, _) => Expr::If(c, t, e, true),
        Expr::BinOp(e1, op, e2, _) => Expr::BinOp(e1, op, e2, true),
        Expr::Match(e, n, h, t, c, _) => Expr::Match(e, n, h, t, c, true),
        other => other,
    }
}

// pub trait VariableParser {
//     type Variable: std::fmt::Display + Clone;

//     fn parse_variable(&mut self) -> Result<Self::Variable, String>;
// }

pub trait ParseMode {
    type Variable: std::fmt::Display + Clone;

    fn parse_variable(core: &mut ParserCore) -> Result<Self::Variable, String>;
}

pub struct NamedMode;

impl ParseMode for NamedMode {
    type Variable = Var;

    fn parse_variable(core: &mut ParserCore) -> Result<Var, String> {
        println!("Default parse_variable for NAMED");
        core.next_var()
    }
}

pub struct NamelessMode;

impl ParseMode for NamelessMode {
    type Variable = DBIndex;
    fn parse_variable(core: &mut ParserCore) -> Result<DBIndex, String> {
        println!("Default parse_variable for NAMELESS");
        match core.peek().cloned() {
            Some(Token::Int(n)) if n >= 0 => {
                core.advance();
                Ok(DBIndex(n as usize))
            }
            _ => Err("Expected de Bruijn index.".to_string()),
        }
    }
}

pub trait HasParseMode {
    type Mode: ParseMode;
}

pub trait FromExpr {
    type ExprType;

    // fn as_expr(&self) -> &Self::ExprType;
}

impl FromExpr for NamedExpr {
    type ExprType = NamedExpr;
}

/// A trait for parsing runtime values, now generic over the expression type `E`.
pub trait ValueParser<E, V> : ExpressionParser<E> where
<Self as HasParseMode>::Mode: ParseMode<Variable = E>,
E: Clone + std::fmt::Display + PartialEq,
V: std::fmt::Display + Clone + PartialEq + FromExpr<ExprType = Expr<E>> {
    fn parse_inner_expr(&self, tokens: Vec<Token>) -> Result<V, String>;
}

pub trait ValueParserDefault<ExprCtn, V> : ValueParser<ExprCtn, V> where
<Self as HasParseMode>::Mode: ParseMode<Variable = ExprCtn>,
ExprCtn: Clone + std::fmt::Display + PartialEq,
V: std::fmt::Display + Clone + PartialEq + FromExpr<ExprType = Expr<ExprCtn>>, {
    //fn core(&mut self) -> &mut ParserCore;
    // fn parse_inner_expr(&self, tokens: Vec<Token>) -> Result<E, String>;
    fn parse_value(&mut self) -> Result<Value<V>, String>;
    fn parse_env_list(&mut self) -> Result<Vec<(Var, Value<V>)>, String>;
    fn parse_list_value(&mut self, paren: bool) -> Result<Value<V>, String>;
    fn parse_list_tail(&mut self, left: Value<V>, paren: bool) -> Result<Value<V>, String>;
    fn parse_single_value(&mut self) -> Result<Value<V>, String>;
    fn collect_tokens_until_rbracket(&mut self) -> Vec<Token>;
    fn parse_func_val(&mut self, env: Vec<(Var, Value<V>)>) -> Result<Value<V>, String>;
    fn parse_rec_func_val(&mut self, env: Vec<(Var, Value<V>)>) -> Result<Value<V>, String>;
}

impl<P, E, V> ValueParserDefault<E, V> for P
where
<Self as HasParseMode>::Mode: ParseMode<Variable = E>,
    P: ValueParser<E, V> + ?Sized,
    E: Clone + std::fmt::Display + PartialEq,
    V: std::fmt::Display + Clone + PartialEq + FromExpr<ExprType = Expr<E>>
{
    // fn core(&mut self) -> &mut ParserCore {
    //     todo!("core() not implemented yet");
    // }
    fn parse_value(&mut self) -> Result<Value<V>, String> {
        self.parse_list_value(false)
    }

    fn parse_list_value(&mut self, paren: bool) -> Result<Value<V>, String> {
        let left = self.parse_single_value()?;
        self.parse_list_tail(left, paren)
    }

    fn parse_single_value(&mut self) -> Result<Value<V>, String> {
        match self.core().peek().cloned() {
            Some(Token::Int(n)) => { self.core().advance(); Ok(Value::Int(n)) }
            Some(Token::Bool(b)) => { self.core().advance(); Ok(Value::Bool(b)) }
            Some(Token::LParen) => {
                self.core().advance();
                let saved_pos = self.core().pos();
                if let Ok(env) = self.parse_env_list() {
                    if self.core().peek() == Some(&Token::RParen) {
                        self.core().advance();
                        if self.core().peek() == Some(&Token::LBracket) {
                            self.core().advance();
                             match self.core().peek() {
                                Some(Token::Fun) => return self.parse_func_val(env),
                                Some(Token::Rec) => return self.parse_rec_func_val(env),
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

    fn parse_env_list(&mut self) -> Result<Vec<(Var, Value<V>)>, String> {
        let mut bindings = vec![];
        if self.core().peek() == Some(&Token::RParen) || self.core().peek() == Some(&Token::Turnstile) {
            return Ok(bindings);
        }
        loop {
            let var = self.core().next_var()?;
            self.core().expect(Token::Equals)?;
            let val = self.parse_value()?;
            bindings.push((var, val));
            if self.core().peek() == Some(&Token::Comma) {
                self.core().advance();
            } else {
                break;
            }
        }
        Ok(bindings)
    }

    fn parse_list_tail(&mut self, left: Value<V>, paren: bool) -> Result<Value<V>, String> {
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

    fn parse_func_val(&mut self, env: Vec<(Var, Value<V>)>) -> Result<Value<V>, String> {
        self.core().expect(Token::Fun)?;
        let param = self.core().next_var()?;
        self.core().expect(Token::Arrow)?;
        let body_tokens = self.collect_tokens_until_rbracket();
        let body = self.parse_inner_expr(body_tokens)?;
        Ok(Value::FunVal(param, Box::new(body), env, false))
    }

    fn parse_rec_func_val(&mut self, env: Vec<(Var, Value<V>)>) -> Result<Value<V>, String> {
        self.core().expect(Token::Rec)?;
        let name = self.core().next_var()?;
        self.core().expect(Token::Equals)?;
        self.core().expect(Token::Fun)?;
        let param = self.core().next_var()?;
        self.core().expect(Token::Arrow)?;
        let body_tokens = self.collect_tokens_until_rbracket();
        let body = self.parse_inner_expr(body_tokens)?;
        Ok(Value::RecFunVal(name, param, Box::new(body), env, false))
    }
}

/// A trait that provides default implementations for parsing all common expressions.
pub trait ExpressionParser<V>: HasParseMode
where
    V: std::fmt::Display + Clone,
    <Self as HasParseMode>::Mode: ParseMode<Variable = V>,
{
    // Each implementor must provide access to its core and a way to get a variable.
    fn core(&mut self) -> &mut ParserCore;

    fn parse_int(&mut self) -> Result<Expr<V>, String> {
        let token = self.core().peek().cloned();
        match token {
            Some(Token::Int(n)) => {
                self.core().advance();
                Ok(Expr::Int(n))
            }
            _ => panic!("Expected int"),
        }
    }
    
    // All expression parsing methods are now default methods on this trait.
    // They operate on the required `core` method.
    fn parse_expr(&mut self) -> Result<Expr<V>, String> {
        match self.core().peek() {
            Some(Token::Let) => self.parse_let_expr(),
            Some(Token::If) => self.parse_if_expr(),
            Some(Token::Fun) => self.parse_fun_expr(),
            Some(Token::Match) => self.parse_match_expr(),
            _ => self.parse_lt(),
        }
    }
    
    fn parse_let_expr(&mut self) -> Result<Expr<V>, String> {
        self.core().advance(); // consume 'let'
        if self.core().peek() == Some(&Token::Rec) {
            self.core().advance(); // consume 'rec'
            let func_name = self.core().next_var()?;
            self.core().expect(Token::Equals)?;
            let body = self.parse_expr()?;
            self.core().expect(Token::In)?;
            let cont = self.parse_expr()?;
            if let Expr::Fun(param, fun_body, _) = body {
                 Ok(Expr::LetRec(func_name, param, fun_body, Box::new(cont), false))
            } else { Err("Expected a function definition after 'let rec ='".to_string()) }
        } else {
            let var = self.core().next_var()?;
            self.core().expect(Token::Equals)?;
            let bound_expr = self.parse_expr()?;
            self.core().expect(Token::In)?;
            let cont = self.parse_expr()?;
            Ok(Expr::Let(var, Box::new(bound_expr), Box::new(cont), false))
        }
    }

    fn parse_if_expr(&mut self) -> Result<Expr<V>, String> {
        self.core().advance(); // consume 'if'
        let cond = self.parse_expr()?;
        self.core().expect(Token::Then)?;
        let then_branch = self.parse_expr()?;
        self.core().expect(Token::Else)?;
        let else_branch = self.parse_expr()?;
        Ok(Expr::If(Box::new(cond), Box::new(then_branch), Box::new(else_branch), false))
    }

    fn parse_fun_expr(&mut self) -> Result<Expr<V>, String> {
        self.core().advance(); // consume 'fun'
        let param = self.core().next_var()?;
        self.core().expect(Token::Arrow)?;
        let body = self.parse_expr()?;
        Ok(Expr::Fun(param, Box::new(body), false))
    }

    fn parse_match_expr(&mut self) -> Result<Expr<V>, String> {
        self.core().advance(); // consume 'match'
        let expr_to_match = self.parse_expr()?;
        self.core().expect(Token::With)?;
        self.core().expect(Token::Nil)?;
        self.core().expect(Token::Arrow)?;
        let nil_case = self.parse_expr()?;
        self.core().expect(Token::Bar)?;
        let head_var = self.core().next_var()?;
        self.core().expect(Token::ColonColon)?;
        let tail_var = self.core().next_var()?;
        self.core().expect(Token::Arrow)?;
        let cons_case = self.parse_expr()?;
        Ok(Expr::Match(Box::new(expr_to_match), Box::new(nil_case), head_var, tail_var, Box::new(cons_case), false))
    }

    fn parse_lt(&mut self) -> Result<Expr<V>, String> {
        let mut lhs = self.parse_cons()?;
        while self.core().peek() == Some(&Token::Lt) {
            self.core().advance();
            let rhs = self.parse_cons()?;
            lhs = Expr::BinOp(Box::new(lhs), Op::Lt, Box::new(rhs), false);
        }
        Ok(lhs)
    }
    
    fn parse_cons(&mut self) -> Result<Expr<V>, String> {
        let mut lhs = self.parse_add_sub()?;
        if self.core().peek() == Some(&Token::ColonColon) {
            self.core().advance();
            let rhs = self.parse_cons()?;
            lhs = Expr::BinOp(Box::new(lhs), Op::Cons, Box::new(rhs), false);
        }
        Ok(lhs)
    }

    fn parse_add_sub(&mut self) -> Result<Expr<V>, String> {
        let mut lhs = self.parse_mul()?;
        while let Some(op_token) = self.core().peek() {
            let op = match op_token {
                Token::Plus => Op::Add, Token::Minus => Op::Sub, _ => break,
            };
            self.core().advance();
            let rhs = self.parse_mul()?;
            lhs = Expr::BinOp(Box::new(lhs), op, Box::new(rhs), false);
        }
        Ok(lhs)
    }

    fn parse_mul(&mut self) -> Result<Expr<V>, String> {
        let mut lhs = self.parse_app()?;
        while self.core().peek() == Some(&Token::Star) {
            self.core().advance();
            let rhs = self.parse_app()?;
            lhs = Expr::BinOp(Box::new(lhs), Op::Mul, Box::new(rhs), false);
        }
        Ok(lhs)
    }

    fn parse_app(&mut self) -> Result<Expr<V>, String> {
        let mut lhs = self.parse_atom()?;
        while let Some(tok) = self.core().peek() {
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

    fn parse_atom(&mut self) -> Result<Expr<V>, String> {
        match self.core().peek().cloned() {
            Some(Token::Minus) => {
                self.core().advance();
                match self.core().peek().cloned() {
                    Some(Token::Int(n)) => {
                        self.core().advance();
                        Ok(Expr::Int(-(n)))
                    }
                    _ => {
                        // Unary minus should apply to the next full term, not just an atom.
                        let expr = self.parse_app()?;
                        Ok(Expr::BinOp(Box::new(Expr::Int(0)), Op::Sub, Box::new(expr), false))
                    }
                }
            }
            Some(Token::If) => self.parse_if_expr(),
            Some(Token::Let) => self.parse_let_expr(),
            Some(Token::Ident(_)) => {
                let var = <Self::Mode as ParseMode>::parse_variable(self.core())?;
                Ok(Expr::Var(var))
            },
            Some(Token::Int(_)) => self.parse_int(),
            Some(Token::Bool(b)) => { 
                self.core().advance(); Ok(Expr::Bool(b)) 
            }
            Some(Token::LParen) => {
                self.core().advance();
                let expr = self.parse_expr()?;
                self.core().expect(Token::RParen)?;
                Ok(mark_expr_paren(expr))
            }
            Some(Token::Fun) => self.parse_fun_expr(),
            Some(Token::Nil) => {
                self.core().advance();
                Ok(Expr::Nil)
            },
            Some(Token::Match) => self.parse_match_expr(),
            _ => Err("Unexpected token found at atomic level.".to_string()),
        }
    }
}

/// A trait for parsers that need to handle type syntax.
pub trait TypeParser {
    // Each implementor must provide access to its core and a way to handle type variables.
    fn core(&mut self) -> &mut ParserCore;
    fn parse_single_type(&mut self) -> Result<Type, String>;

    /// Parses a potentially complex type, like `int -> int` or `(int -> int) list`.
    fn parse_type(&mut self) -> Result<Type, String> {
        let mut ty = self.parse_single_type()?;
        if self.core().peek() == Some(&Token::Arrow) {
            self.core().advance();
            let return_ty = self.parse_type()?;
            ty = Type::Fun(Box::new(ty), Box::new(return_ty));
        }
        Ok(ty)
    }
}