use crate::{common::{ast::{DBIndex, Expr, NamedVar, NamelessVar, Op, Value, Variable}, tokenizer::Token}, parser::{delegate::ParseTarget, primitive::{traits::{ParseableVariable, VariableParsing}, BoolParsing, IntParsing, NilParsing}, value::ValueParserDefault, BaseParser, ValueParser}};

pub trait ExpressionParser<V> : BaseParser<V = V> 
where V: Variable {
    fn parse_expr(&mut self) -> Result<Expr<V>, String>;
    fn parse_expr_atom(&mut self) -> Result<Expr<V>, String>;
}

// BinOp Parsing Traits
pub trait AddExprParsing<V> : ExpressionParser<V> + IntParsing<Expr<V>>
where V: Variable {
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

pub trait SubExprParsing<V> : ExpressionParser<V> + IntParsing<Expr<V>>
where V: Variable {
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

pub trait MulExprParsing<V> : ExpressionParser<V> + IntParsing<Expr<V>>
where V: Variable {
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

pub trait  LtExprParsing<V> : ExpressionParser<V> + IntParsing<Expr<V>> + BoolParsing<Expr<V>>
where V: Variable {
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

pub trait ConsExprParsing<V> : ExpressionParser<V> + NilParsing<Expr<V>>
where V: Variable {
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

pub trait AppExprParsing<V> : ExpressionParser<V> + VariableParsing<Expr<V>>
where V: Variable + ParseableVariable {
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
pub trait IfExprParsing<V> : ExpressionParser<V> + BoolParsing<Expr<V>>
where V: Variable {
    fn check(token: &Token) -> bool { matches!(token, Token::If) }

    fn parse(&mut self) -> Result<Expr<Self::V>, String> {
        self.core().advance(); // consume 'if'
        let cond = self.parse_expr()?;
        self.core().expect(Token::Then)?;
        let then_branch = self.parse_expr()?;
        self.core().expect(Token::Else)?;
        let else_branch = self.parse_expr()?;
        Ok(Expr::If(Box::new(cond), Box::new(then_branch), Box::new(else_branch)))
    }
}

pub trait LetExprParsing<V> : ExpressionParser<V> + VariableParsing<Expr<V>>
where V: Variable + ParseableVariable {
    fn check(token: &Token) -> bool { matches!(token, Token::Let) }

    fn parse(&mut self) -> Result<Expr<Self::V>, String>
    where
        Self::V: ParseableVariable {
        self.core().advance(); // consume 'let'
        let var = <Self as VariableParsing<Expr<V>>>::parse(self)?.into_variable().ok_or("Expected a variable name in `let` expression, but found something else.")?;
        self.core().expect(Token::Equals)?;
        let bound_expr = self.parse_expr()?;
        self.core().expect(Token::In)?;
        let cont = self.parse_expr()?;
        Ok(Expr::Let(var, Box::new(bound_expr), Box::new(cont)))
    }
}
pub trait FunExprParsing<V> : ExpressionParser<V> + VariableParsing<Expr<V>>
where V: Variable + ParseableVariable {
    fn check(token: &Token) -> bool { matches!(token, Token::Fun) }

    fn parse(&mut self) -> Result<Expr<Self::V>, String>     where
        Self::V: ParseableVariable {
        self.core().advance(); // consume 'fun'
        let param = <Self as VariableParsing<Expr<V>>>::parse(self)?.into_variable().ok_or("Expected a variable name in `let` expression, but found something else.")?;
        self.core().expect(Token::Arrow)?;
        let body = self.parse_expr()?;
        Ok(Expr::Fun(param, Box::new(body)))
    }
}

pub trait RecFunExprParsing<V> : ExpressionParser<V> + FunExprParsing<V>
where V: Variable + ParseableVariable {
    fn check(token: &Token) -> bool { matches!(token, Token::LetRec) }

    fn parse(&mut self) -> Result<Expr<Self::V>, String>     where
        Self::V: ParseableVariable {
        self.core().advance(); // consume 'let rec'
        let func_name = <Self as VariableParsing<Expr<V>>>::parse(self)?.into_variable().ok_or("Expected a variable name in `let` expression, but found something else.")?;
        self.core().expect(Token::Equals)?;
        let body = self.parse_expr()?;
        self.core().expect(Token::In)?;
        let cont = self.parse_expr()?;
        if let Expr::Fun(param, fun_body) = body {
            Ok(Expr::LetRec(func_name, param, fun_body, Box::new(cont)))
        } else { 
            Err("Expected a function definition after 'let rec ='".to_string()) 
        }
    }
}

pub trait MatchExprParsing<V>: ExpressionParser<V> + ConsExprParsing<V> + VariableParsing<Expr<V>>
where V: Variable + ParseableVariable {
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
        let head_var = <Self as VariableParsing<Expr<V>>>::parse(self)?.into_variable().ok_or("Expected a variable name in `let` expression, but found something else.")?;
        self.core().expect(Token::ColonColon)?;
        let tail_var = <Self as VariableParsing<Expr<V>>>::parse(self)?.into_variable().ok_or("Expected a variable name in `let` expression, but found something else.")?;
        self.core().expect(Token::Arrow)?;
        let cons_case = self.parse_expr()?;
        Ok(Expr::Match(Box::new(expr_to_match), Box::new(nil_case), head_var, tail_var, Box::new(cons_case)))
    }
}
