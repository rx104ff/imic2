use crate::common::ast::{core::Variable, env::Env, expr::Expr};

pub trait HasExpr { 
    type V: Variable; 
    fn expr(&self) -> &Expr<Self::V>; 
}

pub trait HasEnv { 
    type V: Variable; 
    type T; 
    fn env(&self) -> &Env<Self::V, Self::T>; 
}

pub trait HasResult { 
    type R; 
    fn result(&self) -> &Self::R; 
}

pub trait FromParts: Sized {
    type V: Variable; 
    type T; 
    type R;
    fn from_parts(env: Env<Self::V, Self::T>, expr: Expr<Self::V>, result: Self::R) -> Self;
}
