use crate::{common::{ast::{DBIndex, Expr, NamedVar, NamelessVar, Op, Value, Variable}, tokenizer::Token}, parser::{BaseParser, ValueParser, value::ValueParserDefault}};

pub trait ExpressionParser : BaseParser {
    fn parse_expr(&mut self) -> Result<Expr<Self::V>, String>;
    fn parse_atom(&mut self) -> Result<Expr<Self::V>, String>;
}


/// Primitive Parsing Traits
pub trait IntParsing : BaseParser {
    fn check(token: &Token) -> bool { matches!(token, Token::Int(_)) }

    fn parse(&mut self) -> Result<Expr<Self::V>, String> {
        let token = self.core().peek().cloned();
        match token {
            Some(Token::Int(n)) => {
                self.core().advance();
                Ok(Expr::Int(n))
            }
            _ => panic!("Expected int"),
        }
    }
}

pub trait BoolParsing : BaseParser {
    fn check(token: &Token) -> bool { matches!(token, Token::Bool(_)) }

    fn parse(&mut self) -> Result<Expr<Self::V>, String> {
        let token = self.core().peek().cloned();
        match token {
            Some(Token::Bool(b)) => {
                self.core().advance();
                Ok(Expr::Bool(b))
            }
            _ => panic!("Expected bool"),
        }
    }
}

pub trait NilParsing : BaseParser {
    fn check(token: &Token) -> bool { matches!(token, Token::Nil) }

    fn handle(&mut self) -> Option<Token> {
        if self.core().peek() == Some(&Token::Nil) {
            self.core().advance();
            return Some(Token::Nil)
        }
        None
    }
    
    fn parse(&mut self) -> Result<Expr<Self::V>, String> {
        let token = self.core().peek().cloned();
        match token {
            Some(Token::Nil) => {
                self.core().advance();
                Ok(Expr::Nil)
            }
            _ => panic!("Expected nil"),
        }
    }
}


//--------------------------------------------------------------------//
// 1. Helper Trait
//--------------------------------------------------------------------//
/// Defines the logic for how a specific variable type (like `NamedVar` or
/// `NamelessVar`) should be created from a token.
pub trait ParseableVariable: Sized {
    /// Tries to create a variable from the given token.
    /// Returns `Some(variable)` on success or `None` if the token
    /// does not represent a valid variable of this type.
    fn check(token: &Token) -> bool;
    fn from_token(token: &Token) -> Option<Self>;
    fn has_explicit_bindings() -> bool;
}


//--------------------------------------------------------------------//
// 2. Main `VariableParser` Trait
//--------------------------------------------------------------------//
/// The main trait for parsing variables. The `build_parser!` macro will
/// generate `impl VariableParser for Parser {}`.
pub trait VariableParser: BaseParser {
    /// This method now has a default implementation that works for any
    /// variable type that implements our `ParseableVariable` helper trait.
    fn check(token: &Token) -> bool
    where Self::V: ParseableVariable 
    { 
        Self::V::from_token(token).is_some()
    }

    fn parse(&mut self) -> Result<Expr<Self::V>, String>
    where
        Self::V: ParseableVariable, // This bound connects the two traits
    {
        // Peek at the next token without consuming it.
        let token = self.core().peek().ok_or_else(|| "Unexpected end of input while parsing variable".to_string())?;
        //print!("{:?}",token);

        // Use the helper trait to try to create the variable.
        if let Some(var) = Self::V::from_token(token) {
            self.core().advance(); // Success! Consume the token.
            Ok(Expr::Var(var))
        } else {
            // The token was not a valid variable of the expected type.
            Err(format!("Token {:?} cannot be parsed as a variable.", token))
        }
    }

    /// Parses a list of environment bindings. The logic dispatched is based on the `var_type`.
    fn parse_env_list(&mut self) -> Result<Vec<(Self::V, Value<Self::V>)>, String>
    where
        Self: ValueParser, // Needed to call `self.parse_value()`
        Self::V: ParseableVariable + Clone, // `Clone` is needed for the nameless var logic
    {
        // Handle empty environment case for both types
        if self.core().peek() == Some(&Token::RParen) || self.core().peek() == Some(&Token::Turnstile) {
            return Ok(vec![]);
        }

        // Dispatch to the correct parsing logic based on the variable type.
        if Self::V::has_explicit_bindings() {
            // --- Logic for Named Variables (`var = val`) ---
            let mut bindings = vec![];
            loop {
                let var = self.parse()?.into_variable().ok_or("Expected a variable name in `let` expression, but found something else.")?;
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
        } else {
            // --- Logic for Nameless Variables (list of values) ---
            let mut values = vec![];
            loop {
                let val = self.parse_value()?;
                values.push(val);

                if self.core().peek() == Some(&Token::Comma) {
                    self.core().advance();
                } else {
                    break;
                }
            }
            // Create bindings with the correct placeholder binder.
            let bindings = values
                .into_iter()
                .filter_map(|val| {
                    // The binder is a placeholder created from the `.` token.
                    Self::V::from_token(&Token::Dot).map(|binder| (binder, val))
                })
                .collect();
            Ok(bindings)
        }
    }
}


//--------------------------------------------------------------------//
// 3. Implementation for `NamedVar`
//--------------------------------------------------------------------//
/// Implements the variable creation logic for named variables.
impl ParseableVariable for NamedVar {
    fn check(token: &Token) -> bool {
        if let Token::Ident(_) = token {
            true
        } else {
            false
        }
    }

    fn from_token(token: &Token) -> Option<Self> {
        // A NamedVar is created from an `Ident` token.
        if let Token::Ident(name) = token {
            Some(NamedVar(name.clone()))
        } else {
            None // Any other token is not a named variable.
        }
    }

    fn has_explicit_bindings() -> bool {
        true // Named variables use `var = val`.
    }
}


//--------------------------------------------------------------------//
// 4. Implementation for `NamelessVar`
//--------------------------------------------------------------------//
/// Implements the variable creation logic for de Bruijn indexed variables.
impl ParseableVariable for NamelessVar {
    fn check(token: &Token) -> bool {
        match token {
            Token::HashVar(_) => true,
            Token::Dot => true, // Special case for the dot syntax
            _ => false, // Any other token is not a nameless variable.
        }
    }
    fn from_token(token: &Token) -> Option<Self> {
        // A NamelessVar is created from either a `#` or `.` token.
        match token {
            Token::HashVar(n) => Some(NamelessVar(DBIndex(*n as usize))),
            Token::Dot => Some(NamelessVar(DBIndex(0))), // Special case for the dot syntax
            _ => None, // Any other token is not a nameless variable.
        }
    }

    fn has_explicit_bindings() -> bool {
        false // Named variables use `var = val`.
    }
}

pub trait GroupParsing : ExpressionParser {
    fn check(token: &Token) -> bool { matches!(token, Token::LParen) }

    fn parse(&mut self) -> Result<Expr<Self::V>, String> {
        self.core().advance();
        let expr = self.parse_expr()?;
        self.core().expect(Token::RParen)?;
        Ok(mark_expr_paren(expr))
    }
}

pub trait UnaryParsing: BaseParser {
    fn parse_unary(&mut self) -> Result<Expr<Self::V>, String>;
}

pub trait UnaryOpParser: UnaryParsing {
    // Checks if the current token can be the start of this unary expression
    fn check(token: &Token) -> bool;

    // Parses the expression
    fn parse(&mut self) -> Result<Expr<Self::V>, String>;
}

// An example implementation for unary minus
pub trait UnaryMinusParser: UnaryParsing {
    fn check(token: &Token) -> bool {
        matches!(token, Token::Minus)
    }

    fn parse(&mut self) -> Result<Expr<Self::V>, String> {
        self.core().advance(); // Consume the '-' token
        // Recursively call the unary parsing function for the operand.
        // This correctly handles expressions like `- - 5` or `-(2+3)`.
        let operand = self.parse_unary()?;
        Ok(Expr::UnaryOp(Op::Sub, Box::new(operand), false))
    }
}

// BinOp Parsing Traits
pub trait AddExprParsing : ExpressionParser + IntParsing {
    fn check (token: &Token) -> bool {
        matches!(token, Token::Plus)
    }

    fn handle(&mut self) -> Option<Op> {
        if self.core().peek() == Some(&Token::Plus) {
            self.core().advance();
            return Some(Op::Add)
        }
        None
    }

    fn is_right_assoc() -> bool { false }
}

pub trait SubExprParsing : ExpressionParser + IntParsing {
    fn check (token: &Token) -> bool {
        matches!(token, Token::Minus)
    }

    fn handle(&mut self) -> Option<Op> {
        if self.core().peek() == Some(&Token::Minus) {
            self.core().advance();
            return Some(Op::Sub)
        }
        None
    }

    fn is_right_assoc() -> bool { false }
}

pub trait MulExprParsing : ExpressionParser + IntParsing {
    fn check (token: &Token) -> bool {
        matches!(token, Token::Star)
    }

    fn handle(&mut self) -> Option<Op> {
        if self.core().peek() == Some(&Token::Star) {
            self.core().advance();
            return Some(Op::Mul)
        }
        None
    }

    fn is_right_assoc() -> bool { false }
}

pub trait  LtExprParsing : ExpressionParser + IntParsing + BoolParsing {
    fn check (token: &Token) -> bool {
        matches!(token, Token::Lt)
    }

    fn handle(&mut self) -> Option<Op> {
        if self.core().peek() == Some(&Token::Lt) {
            self.core().advance();
            return Some(Op::Lt)
        }
        None
    }

    fn is_right_assoc() -> bool { false }
}

pub trait ConsExprParsing : ExpressionParser + NilParsing {
    fn check (token: &Token) -> bool {
        matches!(token, Token::ColonColon)
    }

    fn handle(&mut self) -> Option<Op> {
        if self.core().peek() == Some(&Token::ColonColon) {
            self.core().advance();
            return Some(Op::Cons)
        }
        None
    }

    fn is_right_assoc() -> bool { true }
}

pub trait AppExprParsing : ExpressionParser + VariableParser {
    fn check(token: &Token) -> bool { 
        matches!(token, Token::Int(_) | Token::Ident(_) | Token::Bool(_) | Token::Dot | Token::HashVar(_) | Token::LParen | Token::Nil)
     }

    fn handle(&mut self) -> Option<Op> {
        let token = self.core().peek();
        if matches!(token,Some(Token::Int(_) | Token::Ident(_) | Token::Bool(_) | Token::Dot | Token::HashVar(_) | Token::LParen | Token::Nil)) {
            Some(Op::App)
        } else {
            None
        }
    }

    fn is_right_assoc() -> bool { false }
}

// Dispatch Parsing Traits
pub trait IfExprParsing : ExpressionParser + BoolParsing {
    fn check(token: &Token) -> bool { matches!(token, Token::If) }

    fn parse(&mut self) -> Result<Expr<Self::V>, String> {
        self.core().advance(); // consume 'if'
        let cond = self.parse_expr()?;
        self.core().expect(Token::Then)?;
        let then_branch = self.parse_expr()?;
        self.core().expect(Token::Else)?;
        let else_branch = self.parse_expr()?;
        Ok(Expr::If(Box::new(cond), Box::new(then_branch), Box::new(else_branch), false))
    }
}

pub trait LetExprParsing : ExpressionParser + VariableParser {
    fn check(token: &Token) -> bool { matches!(token, Token::Let) }

    fn parse(&mut self) -> Result<Expr<Self::V>, String>
    where
        Self::V: ParseableVariable {
        self.core().advance(); // consume 'let'
        let var = <Self as VariableParser>::parse(self)?.into_variable().ok_or("Expected a variable name in `let` expression, but found something else.")?;
        self.core().expect(Token::Equals)?;
        let bound_expr = self.parse_expr()?;
        self.core().expect(Token::In)?;
        let cont = self.parse_expr()?;
        Ok(Expr::Let(var, Box::new(bound_expr), Box::new(cont), false))
    }
}
pub trait FunExprParsing : ExpressionParser + VariableParser {
    fn check(token: &Token) -> bool { matches!(token, Token::Fun) }

    fn parse(&mut self) -> Result<Expr<Self::V>, String>     where
        Self::V: ParseableVariable {
        self.core().advance(); // consume 'fun'
        let param = <Self as VariableParser>::parse(self)?.into_variable().ok_or("Expected a variable name in `let` expression, but found something else.")?;
        self.core().expect(Token::Arrow)?;
        let body = self.parse_expr()?;
        Ok(Expr::Fun(param, Box::new(body), false))
    }
}

pub trait RecFunExprParsing : ExpressionParser + FunExprParsing{
    fn check(token: &Token) -> bool { matches!(token, Token::LetRec) }

    fn parse(&mut self) -> Result<Expr<Self::V>, String>     where
        Self::V: ParseableVariable {
        self.core().advance(); // consume 'let rec'
        let func_name = <Self as VariableParser>::parse(self)?.into_variable().ok_or("Expected a variable name in `let` expression, but found something else.")?;
        self.core().expect(Token::Equals)?;
        let body = self.parse_expr()?;
        self.core().expect(Token::In)?;
        let cont = self.parse_expr()?;
        if let Expr::Fun(param, fun_body, _) = body {
            Ok(Expr::LetRec(func_name, param, fun_body, Box::new(cont), false))
        } else { 
            Err("Expected a function definition after 'let rec ='".to_string()) 
        }
    }
}

pub trait MatchExprParsing: ExpressionParser + ConsExprParsing + VariableParser {
    fn check(token: &Token) -> bool { matches!(token, Token::Match) }

    fn parse(&mut self) -> Result<Expr<Self::V>, String>     where
        Self::V: ParseableVariable {
        self.core().advance(); // consume 'match'
        let expr_to_match = self.parse_expr()?;
        self.core().expect(Token::With)?;
        self.core().expect(Token::Nil)?;
        self.core().expect(Token::Arrow)?;
        let nil_case = self.parse_expr()?;
        self.core().expect(Token::Bar)?;
        let head_var = <Self as VariableParser>::parse(self)?.into_variable().ok_or("Expected a variable name in `let` expression, but found something else.")?;
        self.core().expect(Token::ColonColon)?;
        let tail_var = <Self as VariableParser>::parse(self)?.into_variable().ok_or("Expected a variable name in `let` expression, but found something else.")?;
        self.core().expect(Token::Arrow)?;
        let cons_case = self.parse_expr()?;
        Ok(Expr::Match(Box::new(expr_to_match), Box::new(nil_case), head_var, tail_var, Box::new(cons_case), false))
    }
}

/// A helper function to mark an expression as having been parsed inside parentheses.
fn mark_expr_paren<V>(expr: Expr<V>) -> Expr<V> where V: Variable
{
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