use std::fmt;

use crate::common::{ast::{core::NamedVar, expr::NamedExpr, r#type::{PolyTypeEnv, Type}}, proof::JudgmentStyle};

impl JudgmentStyle for Type<NamedVar> {
    fn binding_separator(&self) -> &'static str { ":" }
    fn judgment_verb(&self) -> &'static str { ":" }
    fn axiom_verb(&self) -> &'static str { ":" }
}
