use std::fmt;

use crate::common::ast::{ArithmeticOp, Judgment, ReductionType};

// A structure to represent a formal proof in the Nat system
#[derive(Debug)]
pub struct Derivation {
    pub conclusion: Judgment,
    pub rule: String,
    pub premises: Vec<Derivation>,
}

impl fmt::Display for Judgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Judgment::Arithmetic { op, n1, n2, n3 } => {
                let op_str = match op {
                    ArithmeticOp::Plus => "plus",
                    ArithmeticOp::Times => "times",
                };
                write!(f, "{} {} {} is {}", n1, op_str, n2, n3)
            }
            Judgment::Comparison { n1, n2 } => write!(f, "{} is less than {}", n1, n2),
            Judgment::Evaluation { exp, n } => write!(f, "{} evalto {}", exp, n),
            Judgment::Reduction { r_type, e1, e2 } => {
                let arrow = match r_type {
                    ReductionType::Single => "--->",
                    ReductionType::Direct => "-d->",
                    ReductionType::Multi => "-*->",
                };
                write!(f, "{} {} {}", e1, arrow, e2)
            },
            _ => Ok(()),
        }
    }
}

impl fmt::Display for Derivation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn fmt_with_indent(d: &Derivation, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
            let prefix = "  ".repeat(indent);
            writeln!(f, "{}{} by {} {{", prefix, d.conclusion, d.rule)?;
            for premise in &d.premises {
                fmt_with_indent(premise, f, indent + 1)?;
            }
            writeln!(f, "{}}};", prefix)
        }
        fmt_with_indent(self, f, 0)
    }
}
