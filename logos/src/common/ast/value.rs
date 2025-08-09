use crate::common::ast::{core::{FromBool, FromGroup, FromInt, FromNil, FromUnaryOp, NamedVar, NamelessVar, Op, Variable}, env::{Env, EnvDisplay}, expr::Expr};
use std::fmt::{self, Debug, Display};

#[derive(Debug, Clone, PartialEq)]
pub enum Value<V: Variable> {
    Int(i64),
    Bool(bool),
    Nil,
    Cons(Box<Value<V>>, Box<Value<V>>),
    FunVal(V, Box<Expr<V>>, Env<V, Value<V>>),
    RecFunVal(V, V, Box<Expr<V>>, Env<V, Value<V>>),
    Group(Box<Value<V>>)
}

pub type NamedValue = Value<NamedVar>;

pub type NamelessValue = Value<NamelessVar>;

impl<V: Variable> Value<V> {
    // This no longer needs the problematic trait bound
    fn fmt_logic(&self, f: &mut fmt::Formatter<'_>, is_env_context: bool) -> fmt::Result {
        match self {
            Value::Int(i) => write!(f, "{}", i),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "[]"),
            Value::FunVal(param, body, _env) => {
                // In an environment, show a placeholder. Otherwise, show the function signature.
                if is_env_context {
                    write!(f, "<fun>")
                } else {
                    // NEVER display the captured environment here. This is the key.
                    write!(f, "fun {} -> {}", param, body)
                }
            }
            Value::RecFunVal(func, param, body, _env) => {
                // In an environment, show a placeholder. Otherwise, show the function signature.
                if is_env_context {
                    write!(f, "<rec fun>")
                } else {
                     // NEVER display the captured environment here.
                    write!(f, "rec {} = fun {} -> {}", func, param, body)
                }
            }
            Value::Cons(h, t) => {
                if is_env_context {
                    write!(f, "[..]") // Placeholder for lists in an env
                } else {
                    write!(f, "{}::{}", h, t)
                }
            }
            Value::Group(v) => {
                write!(f, "(")?;
                v.fmt_logic(f, is_env_context)?;
                write!(f, ")")
            }
        }
    }
}

// The Display impl is now clean and has no complex bounds.
impl<V: Variable> Display for Value<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_logic(f, false)
    }
}

// The EnvDisplay impl is also clean.
impl<V: Variable> EnvDisplay for Value<V> {
    fn fmt_for_env(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_logic(f, true)
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
