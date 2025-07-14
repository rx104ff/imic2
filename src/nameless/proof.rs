// src/nameless_eval/proof.rs
use crate::common::ast::{Expr, NamelessEnv, NamelessExpr, NamelessValue, Op};
use std::fmt;

pub trait Axiom {
    fn to_axiom_string(&self) -> Option<String>;
}

impl Axiom for Derivation {
    fn to_axiom_string(&self) -> Option<String> {
        if !self.rule.starts_with("B-") {
            return None;
        }

        if let Expr::BinOp(lhs_expr, op, rhs_expr, _) = &self.expr {
            // Since this is an axiom for basic operations, the expressions
            // inside must be simple values. We extract them.
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
                Op::Cons => "cons",
            };

            return Some(format!(
                "{} {} {} is {} by {} {{}};",
                lhs_val,
                op_word,
                rhs_val,
                self.result,
                self.rule
            ));
        }
        None
    }
}

pub struct Derivation {
    pub env: NamelessEnv,
    pub expr: NamelessExpr,
    pub result: NamelessValue,
    pub rule: String,
    pub sub_derivations: Vec<Derivation>,
}

fn format_env(env: &NamelessEnv) -> String {
    if env.is_empty() {
        "|- ".to_string()
    } else {
        let binds = env
            .iter()
            .map(|val| format!("{}", val.1))
            .collect::<Vec<_>>()
            .join(", ");
        format!("{} |- ", binds)
    }
}

impl fmt::Display for Derivation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn fmt_with_indent(d: &Derivation, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
            let prefix = "    ".repeat(indent);

            if let Some(ax_str) = d.to_axiom_string() {
                writeln!(f, "{}{}", prefix, ax_str)?;
                return Ok(());
            }

            writeln!(
                f,
                "{}{}{} evalto {} by {} {{",
                prefix,
                format_env(&d.env),
                d.expr,
                d.result,
                d.rule
            )?;
            for premise in &d.sub_derivations {
                fmt_with_indent(premise, f, indent + 1)?;
            }
            writeln!(f, "{}}};", prefix)
        }
        fmt_with_indent(self, f, 0)
    }
}