use crate::common::ast::{core::{FromBool, FromGroup, FromInt, FromNil, FromUnaryOp, FromVar, NamedVar, NamelessVar, Op, Variable}, nat::Nat};
use std::fmt::{self, Debug};


#[derive(Debug, Clone, PartialEq)]
pub enum Expr<V: Variable> {
    Int(i64),
    Bool(bool),
    Nat(Nat),
    Var(V),
    Nil,
    Let(V, Box<Expr<V>>, Box<Expr<V>>),
    LetRec(V, V, Box<Expr<V>>, Box<Expr<V>>),
    Fun(V, Box<Expr<V>>),
    App(Box<Expr<V>>, Box<Expr<V>>),
    If(Box<Expr<V>>, Box<Expr<V>>, Box<Expr<V>>),
    UnaryOp(Op, Box<Expr<V>>),
    BinOp(Box<Expr<V>>, Op, Box<Expr<V>>),
    Match(Box<Expr<V>>, Box<Expr<V>>, V, V, Box<Expr<V>>),
    Group(Box<Expr<V>>),
    Plus(Box<Expr<V>>, Box<Expr<V>>),
    Times(Box<Expr<V>>, Box<Expr<V>>),
}

impl<V: Variable> Expr<V> {
    pub fn into_variable(self) -> Option<V> {
        if let Expr::Var(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_variable(&self) -> Option<&V> {
        if let Expr::Var(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

pub type NamedExpr = Expr<NamedVar>;

pub type NamelessExpr = Expr<NamelessVar>;

impl<E> fmt::Display for Expr<E> where E: std::fmt::Display + Variable{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Nat(n) => write!(f, "{}", n),
            Expr::Plus(e1, e2) => write!(f, "({} + {})", e1, e2),
            Expr::Times(e1, e2) => write!(f, "({} * {})", e1, e2),
            Expr::Int(n) => write!(f, "{}", n),
            Expr::Bool(b) => write!(f, "{}", b),
            Expr::Var(v) => write!(f, "{}", v),
            Expr::Nil => write!(f, "[]"),
            Expr::Let(x, e1, e2) => {
                let s = format!("let {} = {} in {}", x, e1, e2);
                write!(f, "{}", s)
            },
            Expr::LetRec(func, param, body, cont) => {
                let s = format!("let rec {} = fun {} -> {} in {}", func, param, body, cont);
                write!(f, "{}", s)
            },
            Expr::If(c, t, e) => {
                let s = format!("if {} then {} else {}", c, t, e);
                write!(f, "{}", s)
            },
            Expr::UnaryOp(op, e) => {
                let s = format!("{}{}", op, e);
               write!(f, "{}", s)
            }
            Expr::BinOp(e1, op, e2) => {
                let s = if *op == Op::App {
                    format!("{} {}", e1, e2)
                } else {
                    format!("{} {} {}", e1, op, e2)
                };
                write!(f, "{}", s)
            }
            Expr::Fun(p, b) => {
                let s = format!("fun {} -> {}", p, b);
                write!(f, "{}", s)
            },
            Expr::App(e1, e2) => {
                let s = format!("{} {}", e1, e2);
                write!(f, "{}", s)
            },
            Expr::Match(e, nil_case, x, y, cons_case) => {
                let s = format!("match {} with [] -> {} | {}::{} -> {}", e, nil_case, x, y, cons_case);
                write!(f, "{}", s)
            },
            Expr::Group(e) => {
                let s = format!("({})", e);
                write!(f, "{}", s)
            }
        }
    }
}

impl<V: Variable> FromInt for Expr<V> {
    fn from_int(n: i64) -> Self { Expr::Int(n) }
}

impl<V: Variable> FromBool for Expr<V> {
    fn from_bool(b: bool) -> Self { Expr::Bool(b) }
}

impl<V: Variable> FromNil for Expr<V> {
    fn from_nil() -> Self { Expr::Nil }
}

impl<V: Variable> FromVar<V> for Expr<V> {
    fn from_var(var: V) -> Self {
        Expr::Var(var)
    }
}

impl<V: Variable> FromGroup for Expr<V> {
    fn from_group(inner: Self) -> Self {
        Expr::Group(Box::new(inner))
    }
}

impl<V: Variable> FromUnaryOp for Expr<V> {
    fn from_unary_op(op: Op, operand: Self) -> Result<Self, String> {
        Ok(Expr::UnaryOp(op, Box::new(operand)))
    }
}
