use crate::common::ast::{NamedEnv, NamedExpr, NamedValue};
use crate::eval::version::{LanguageVersion};

pub trait Axiom {
    fn to_axiom_string(&self) -> Option<String>;
}

pub struct Derivation {
    pub env: NamedEnv,
    pub expr: NamedExpr,
    pub result: NamedValue,
    pub rule: String,
    pub sub_derivations: Vec<Derivation>,
    pub version: LanguageVersion,
}
