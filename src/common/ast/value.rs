use crate::common::ast::{core::{FromBool, FromGroup, FromInt, FromNil, FromUnaryOp, NamedVar, NamelessVar, Op, Variable}, expr::Expr};
use std::fmt::{self, Debug};

pub type Env<V> = Vec<(V, Value<V>)>;

#[derive(Debug, Clone, PartialEq)]
pub enum Value<V: Variable> {
    Int(i64),
    Bool(bool),
    Nil,
    Cons(Box<Value<V>>, Box<Value<V>>),
    FunVal(V, Box<Expr<V>>, Env<V>),
    RecFunVal(V, V, Box<Expr<V>>, Env<V>),
    Group(Box<Value<V>>)
}

pub type NamedValue = Value<NamedVar>;

pub type NamelessValue = Value<NamelessVar>;

pub trait DisplayEnv {
    fn display_env(&self) -> String;
}

pub type NamedEnv = Env<NamedVar>;

impl DisplayEnv for NamedEnv {
    fn display_env(&self) -> String {
        if self.is_empty() {
            format!("()")
        } else {
            let parts: Vec<String> = self.iter().map(|(v, val)| format!("{} = {}", v, val)).collect();
            format!("({})", parts.join(", "))
        }
    }
}

pub type NamelessEnv = Env<NamelessVar>;

impl DisplayEnv for NamelessEnv {
    fn display_env(&self) -> String {
        if self.is_empty() {
            format!("()")
        } else {
            let parts: Vec<String> = self.iter().map(|(_, val)| format!("{}", val)).collect();
            format!("({})", parts.join(", "))
        }
    }
}

impl<E: Variable  + 'static> fmt::Display for Value<E> where E: std::fmt::Display,
Env<E>: DisplayEnv, // Add this trait bound
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(i) => write!(f, "{}", i),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "[]"),
            Value::Cons(h, t) => {
                let s = format!("{} :: {}", h, t);
                write!(f, "{}", s)
            }
            Value::FunVal(param, body, env) => {
                write!(f, "{}[fun {} -> {}]", env.display_env(), param, body)
            }
            Value::RecFunVal(func, param, body, env) => {
                write!(f, "{}[rec {} = fun {} -> {}]", env.display_env(), func, param, body)
            }
            Value::Group(v) => {
                write!(f, "({})", v)
            }
        }
    }
}

impl<V: Variable> FromInt for Value<V> {
    fn from_int(n: i64) -> Self { Value::Int(n) }
}

impl<V: Variable> FromBool for Value<V> {
    fn from_bool(b: bool) -> Self { Value::Bool(b) }
}

impl<V: Variable> FromNil for Value<V> {
    fn from_nil() -> Self { Value::Nil }
}

impl<V: Variable> FromGroup for Value<V> {
    fn from_group(inner: Self) -> Self {
        Value::Group(Box::new(inner))
    }
}

impl<V: Variable> FromUnaryOp for Value<V> {
    fn from_unary_op(op: Op, operand: Self) -> Result<Self, String> {
        if op == Op::Sub {
            if let Value::Int(i) = operand {
                return Ok(Value::Int(-i));
            }
        }
        Err(format!("Cannot apply unary operator {:?} to value {:?}", op, operand))
    }
}
