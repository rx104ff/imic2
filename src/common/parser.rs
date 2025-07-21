use crate::common::{ast::{DBIndex, Expr, NamedVar, NamelessVar, Op, Type, Value, Variable}, tokenizer::Token};

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
}

pub trait BaseParser {
    type V: Variable;
    fn core(&mut self) -> &mut ParserCore;
}

pub trait HasHandle {
    //fn handle(&mut self) -> Option<Token>;
    fn check(token: &Token) -> bool;
}

// The base trait for any parser that handles expressions.
pub trait ExpressionParser : BaseParser {
    fn parse_expr(&mut self) -> Result<Expr<Self::V>, String>;
    fn parse_atom(&mut self) -> Result<Expr<Self::V>, String>;
}

#[macro_export]
macro_rules! __internal_build_parser_logic {
    // -- Internal Rule: Recursive Step --
    // This is the main recursive rule. It processes one level of the precedence
    // chain and then calls itself on the rest of the chain.
    (
        // General info passed through the recursion
        parser = $parser_struct:ty,
        var_type = $var:ty,
        primitive_parsers = [ $( $primitive_trait:ident ),* ],
        dispatch_parsers = [ $( $dispatch_trait:ident ),* ],
        first_trait_overall = $first_trait_overall:ident,
        // Accumulator for generated methods
        methods = { $( $methods:tt )* },
        // Accumulator for all trait names that need to be implemented
        all_traits = { $( $all_traits:tt )* },
        // The current level being processed, and the rest of the chain
        chain = [
            { $first_current:ident $(, $rest_current:ident)* }, // Current level
            { $first_next:ident $(, $rest_next:ident)* }       // Next level
            $(, $tail:tt )* // The rest
        ]
    ) => {
        paste::paste! {
            // Recurse, adding the new method and traits to the accumulators.
            $crate::__internal_build_parser_logic! {
                parser = $parser_struct,
                var_type = $var,
                primitive_parsers = [ $( $primitive_trait ),* ],
                dispatch_parsers = [ $( $dispatch_trait ),* ],
                first_trait_overall = $first_trait_overall,
                methods = {
                    $( $methods )*

                    // The new method for the current level. It's named after the first trait.
                    fn [<parse_ $first_current:lower _level>] (&mut self) -> Result<$crate::common::ast::Expr<$var>, String> {
                        // It calls the parser for the *next* level of precedence.
                        let mut lhs = self.[<parse_ $first_next:lower _level>]()?;
                        loop {
                            let maybe_op = {
                                let mut op = None;
                                if op.is_none() { op = <Self as $crate::common::parser::$first_current>::handle(self); }
                                $( if op.is_none() { op = <Self as $crate::common::parser::$rest_current>::handle(self); } )*
                                op
                            };
                            if let Some(op) = maybe_op {
                                let rhs = if <Self as $crate::common::parser::$first_current>::is_right_assoc() {
                                    self.[<parse_ $first_current:lower _level>]()?
                                } else {
                                    self.[<parse_ $first_next:lower _level>]()?
                                };
                                lhs = $crate::common::ast::Expr::BinOp(Box::new(lhs), op, Box::new(rhs), false);
                            } else { break; }
                        }
                        Ok(lhs)
                    }
                },
                all_traits = { $( $all_traits )* $first_current, $( $rest_current, )* },
                // The 'next' level and the tail form the new chain for the next iteration.
                chain = [ { $first_next $(, $rest_next)* } $(, $tail)* ]
            }
        }
    };

    // -- Internal Rule: Base Case --
    // This rule is called for the very last level of the chain. It generates ALL final code.
    (
        parser = $parser_struct:ty,
        var_type = $var:ty,
        primitive_parsers = [ $( $primitive_trait:ident ),* ],
        dispatch_parsers = [ $( $dispatch_trait:ident ),* ],
        first_trait_overall = $first_trait_overall:ident,
        methods = { $( $methods:tt )* },
        all_traits = { $( $all_traits:ident, )* },
        // The chain only has one element left, which triggers this base case.
        chain = [ { $first_current:ident $(, $rest_current:ident)* } ]
    ) => {
        // --- DEFINITIVE FIX: Generate all impls in the correct order here ---

        // 1. Implement all the collected binop traits
        $(
            impl $crate::common::parser::$all_traits for $parser_struct {}
        )*
        // Implement the traits from the final level
        impl $crate::common::parser::$first_current for $parser_struct {}
        $(
            impl $crate::common::parser::$rest_current for $parser_struct {}
        )*

        paste::paste! {
            // 2. Generate the single `impl Parser` block with all chained methods.
            impl $parser_struct {
                $( $methods )*

                // The method for the final level of the chain.
                fn [<parse_ $first_current:lower _level>] (&mut self) -> Result<$crate::common::ast::Expr<$var>, String> {
                    // It calls `parse_atom` to terminate the recursion.
                    let mut lhs = self.parse_atom()?;
                    loop {
                        let maybe_op = {
                            let mut op = None;
                            if op.is_none() { op = <Self as $crate::common::parser::$first_current>::handle(self); }
                            $( if op.is_none() { op = <Self as $crate::common::parser::$rest_current>::handle(self); } )*
                            op
                        };
                        if let Some(op) = maybe_op {
                            let rhs = if <Self as $crate::common::parser::$first_current>::is_right_assoc() {
                                self.[<parse_ $first_current:lower _level>]()?
                            } else {
                                self.parse_atom()?
                            };
                            lhs = $crate::common::ast::Expr::BinOp(Box::new(lhs), op, Box::new(rhs), false);
                        } else { break; }
                    }
                    Ok(lhs)
                }
            }
        }

        // 3. Implement the `ExpressionParser` trait. This is now safe because
        //    the `impl Parser` block with all the `parse_*_level` methods has been defined.
        impl $crate::common::parser::ExpressionParser for $parser_struct {
            fn parse_expr(&mut self) -> Result<$crate::common::ast::Expr<$var>, String> {
                if let Some(token) = self.core().peek() {
                    $(
                        if <Self as $crate::common::parser::$dispatch_trait>::check(token) {
                            return <Self as $crate::common::parser::$dispatch_trait>::parse(self);
                        }
                    )*
                }
                // Call the generated method for the highest precedence level.
                // The name is generated from the `first_trait_overall` we passed down.
                paste::paste! {
                    self.[<parse_ $first_trait_overall:lower _level>]()
                }
            }

            fn parse_atom(&mut self) -> Result<$crate::common::ast::Expr<$var>, String> {
                if let Some(token) = self.core().peek().cloned() {
                    $(
                        if <Self as $crate::common::parser::$dispatch_trait>::check(&token) {
                            return <Self as $crate::common::parser::$dispatch_trait>::parse(self);
                        }
                    )*

                    $(
                        if <Self as $crate::common::parser::$primitive_trait>::check(&token) {
                            return <Self as $crate::common::parser::$primitive_trait>::parse(self);
                        }
                    )*
                }
                Err(format!("Unexpected token at atomic level: {:?}", self.core().peek()))
            }
        }
    };
}


#[macro_export]
macro_rules! build_parser {
    (
        parser = $parser_struct:ty,
        var_type = $var:ty,

        primitive_parsers: [ $( $primitive_trait:ident ),* ],
        dispatch_parsers: [ $( $dispatch_trait:ident ),* ],
        // The signature now destructures the chain to get the first element,
        // which allows us to pass it down to the helper macro.
        binop_chain: [ { $first_trait_in_chain:ident $(, $_rest:ident)* } $(, $rest_chain_entry:tt)* ]
    ) => {
        // Phase 1: Implement the primitive and dispatch traits.
        $(
            impl $crate::common::parser::$primitive_trait for $parser_struct {}
        )*
        $(
            impl $crate::common::parser::$dispatch_trait for $parser_struct {}
        )*

        // Phase 2: Kick off the external, robust recursive macro.
        // This single call generates all binop `impl Trait`, the `impl Parser`,
        // and the `impl ExpressionParser` blocks in the correct order.
        $crate::__internal_build_parser_logic! {
            parser = $parser_struct,
            var_type = $var,
            primitive_parsers = [ $( $primitive_trait ),* ],
            dispatch_parsers = [ $( $dispatch_trait ),* ],
            first_trait_overall = $first_trait_in_chain,
            methods = {},
            all_traits = {},
            // Pass the entire reconstructed chain to the external helper.
            chain = [ { $first_trait_in_chain $(, $_rest)* } $(, $rest_chain_entry)* ]
        }

        // Phase 3: Implement the BaseParser trait.
        impl $crate::common::parser::BaseParser for $parser_struct {
            type V = $var;
            fn core(&mut self) -> &mut $crate::common::parser::ParserCore {
                &mut self.core
            }
        }
    };
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
    fn check(token: &Token) -> bool { true }
    fn parse(&mut self) -> Result<Expr<Self::V>, String>
    where
        Self::V: ParseableVariable, // This bound connects the two traits
    {
        // Peek at the next token without consuming it.
        let token = self.core().peek().ok_or_else(|| "Unexpected end of input while parsing variable".to_string())?;

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
    fn from_token(token: &Token) -> Option<Self> {
        // A NamelessVar is created from either a `#` or `.` token.
        match token {
            Token::HashVar(n) => Some(NamelessVar(DBIndex(*n as usize))),
            Token::Dot => Some(NamelessVar(DBIndex(0))), // Special case for the dot syntax
            _ => None, // Any other token is not a nameless variable.
        }
    }

    fn has_explicit_bindings() -> bool {
        true // Named variables use `var = val`.
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
    fn check(_: &Token) -> bool { true }

    fn handle(&mut self) -> Option<Op> {
        Some(Op::App)
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
    fn check(token: &Token) -> bool { matches!(token, Token::Rec) }

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

pub struct NamedVariableParser;

impl BaseParser for NamedVariableParser {
    type V = NamedVar;

    fn core(&mut self) -> &mut ParserCore {
        todo!()
    }
}

pub trait ValueParser:  ExpressionParser {
    fn parse_inner_expr(&self, tokens: Vec<Token>) -> Result<Expr<Self::V>, String>;
}

pub trait ValueParserDefault: ValueParser {
    // fn parse_env_list(&mut self) -> Result<Vec<(Var, Value<E>)>, String>;
    fn parse_value(&mut self) -> Result<Value<Self::V>, String>;
    fn parse_list_value(&mut self, paren: bool) -> Result<Value<Self::V>, String>;
    fn parse_list_tail(&mut self, left: Value<Self::V>, paren: bool) -> Result<Value<Self::V>, String>;
    fn parse_single_value(&mut self) -> Result<Value<Self::V>, String>;
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
        let left = self.parse_single_value()?;
        self.parse_list_tail(left, paren)
    }

    fn parse_single_value(&mut self) -> Result<Value<Self::V>, String> {
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
