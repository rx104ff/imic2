// src/ast.rs
use std::fmt;
use std::hash::Hash;
use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LanguageVersion {
    ML1,
    ML2,
    ML3,
    ML4,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var(pub String);

impl From<String> for Var {
    fn from(s: String) -> Self {
        Var(s)
    }
}

impl From<&str> for Var {
    fn from(s: &str) -> Self {
        Var(s.to_string())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int(i64),
    Bool(bool),
    Var(Var),
    Let(Var, Box<Expr>, Box<Expr>, bool),
    LetRec(Var, Var, Box<Expr>, Box<Expr>, bool),
    If(Box<Expr>, Box<Expr>, Box<Expr>, bool),
    BinOp(Box<Expr>, Op, Box<Expr>, bool),
    Fun(Var, Box<Expr>, bool),
    App(Box<Expr>, Box<Expr>, bool),
    Nil,
    Cons(Box<Expr>, Box<Expr>, bool),
    Match(Box<Expr>, Box<Expr>, Var, Var, Box<Expr>, bool),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Nil,
    Cons(Box<Value>, Box<Value>, bool),
    FunVal(Var, Box<Expr>, Env, bool),
    RecFunVal(Var, Var, Box<Expr>, Env, bool),
}

pub type Env = Rc<Vec<(Var, Value)>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Lt,
    Cons,
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
