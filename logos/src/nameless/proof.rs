// src/nameless/proof.rs
use std::fmt;

use crate::common::ast::core::{NamelessVar, Op};
use crate::common::ast::expr::Expr;
use crate::common::ast::value::NamelessValue;
// Import the generic structs and traits from common::proof
use crate::common::proof::{Derivation, Judgment, JudgmentStyle};

// Implement the style for nameless evaluation judgments.
impl JudgmentStyle for NamelessValue {
    fn binding_separator(&self) -> &'static str { "=" } // Not used for nameless, but required by trait
    fn judgment_verb(&self) -> &'static str { "evalto" }
    fn axiom_verb(&self) -> &'static str { "is" }
}

pub trait Axiom {
    fn to_axiom_string(&self) -> Option<String>;
}

impl Axiom for Derivation<Judgment<NamelessVar, NamelessValue, NamelessValue>> {
    fn to_axiom_string(&self) -> Option<String> {
        if !self.rule.starts_with("B-") {
            return None;
        }

        if let Expr::BinOp(lhs_expr, op, rhs_expr) = &self.judgment.expr {
            let lhs_val: &NamelessValue = match &**lhs_expr {
                Expr::Int(n) => &NamelessValue::Int(*n),
                _ => return None,
            };
            let rhs_val: &NamelessValue = match &**rhs_expr {
                Expr::Int(n) => &NamelessValue::Int(*n),
                _ => return None,
            };

            let op_word = match op {
                Op::Add => "plus",
                Op::Sub => "minus",
                Op::Mul => "times",
                Op::Lt => "less than",
                _ => "",
            };

            return Some(format!(
                "{} {} {} is {} by {} {{}};",
                lhs_val,
                op_word,
                rhs_val,
                self.judgment.result, // result is on the judgment now
                self.rule
            ));
        }
        None
    }
}

// The old Derivation struct and its Display impl are now completely removed.