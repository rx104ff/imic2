use crate::ast::{Expr, Value, Env, Op};

pub struct Derivation {
    pub env: Env,
    pub expr: Expr,
    pub result: Value,
    pub rule: String,
    pub sub_derivations: Vec<Derivation>,
}