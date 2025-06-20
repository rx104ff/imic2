use crate::ast::{Expr, Value, Env, Op};

pub trait Axiom {
    fn to_axiom_string(&self) -> Option<String>;
}

pub struct Derivation {
    pub env: Env,
    pub expr: Expr,
    pub result: Value,
    pub rule: String,
    pub sub_derivations: Vec<Derivation>,
}