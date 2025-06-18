// src/ast.rs
use std::fmt;
use std::hash::Hash;
use std::rc::Rc;

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
    Let(Var, Box<Expr>, Box<Expr>),
    LetRec(Var, Var, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    BinOp(Box<Expr>, Op, Box<Expr>, bool),
    Fun(Var, Box<Expr>),
    App(Box<Expr>, Box<Expr>, bool),
    Nil,
    Cons(Box<Expr>, Box<Expr>),
    Match(Box<Expr>, Box<Expr>, Var, Var, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Nil,
    Cons(Box<Value>, Box<Value>),
    FunVal(Var, Box<Expr>, Env),
    RecFunVal(Var, Var, Box<Expr>, Env),
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
