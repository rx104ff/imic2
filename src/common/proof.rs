// src/common/proof.rs
use std::fmt::{self, Display};

use crate::common::ast::{core::Variable, env::{Env, FormatStyleFor}, expr::Expr};

/// A generic judgment representing a statement about an expression.
/// - `V`: The type of variable (e.g., NamedVar).
/// - `T`: The type of item stored in the environment (e.g., Value or Type).
/// - `R`: The type of the result (e.g., Value or Type).
#[derive(Debug, Clone, PartialEq)]
pub struct Judgment<V: Variable, T, R: Display> {
    pub env: Env<V, T>,
    pub expr: Expr<V>,
    pub result: R,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Derivation<J> {
    pub judgment: J,
    pub rule: String,
    pub premises: Vec<Derivation<J>>,
}

// --- Styling and Display Traits ---

/// Defines the string components for a judgment's style, driven by the result type.
pub trait JudgmentStyle {
    /// The separator for var/val bindings in the environment (e.g., ":" or "=").
    fn binding_separator(&self) -> &'static str;
    /// The main verb of the judgment (e.g., "evalto" or ":").
    fn judgment_verb(&self) -> &'static str;
    /// The verb for axiom formatting (e.g., "is").
    fn axiom_verb(&self) -> &'static str;
}
pub trait DisplayJudgment {
    fn fmt_judgment(&self, f: &mut fmt::Formatter<'_>, is_axiom: bool) -> fmt::Result;
}

/// The single, generic `DisplayJudgment` implementation for our `Judgment` struct.
impl<V, T, R> DisplayJudgment for Judgment<V, T, R>
where
    V: Variable + Display,
    T: Display, // The environment item type must be displayable.
    R: Display + JudgmentStyle, // The result type must provide a style.
    (): FormatStyleFor<V, T>,
{
    fn fmt_judgment(&self, f: &mut fmt::Formatter<'_>, is_axiom: bool) -> fmt::Result {
        let style = &self.result; // The style comes from the result type.

        if is_axiom {
            write!(f, "{}", &self.env)?;
            write!(f, "{} {} {}", self.expr, style.axiom_verb(), self.result)
        } else {
            // Standard format uses the environment and main separators.
            write!(f, "{}", &self.env)?;
            write!(f, "{} {} {}", self.expr, style.judgment_verb(), self.result)
        }
    }
}

// --- Generic `Display` Implementation for `Derivation` ---

impl<J: DisplayJudgment + Clone> Display for Derivation<J> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt_with_indent(self, f, 0)
    }
}

fn fmt_with_indent<J: DisplayJudgment + Clone>(d: &Derivation<J>, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
    let prefix = "    ".repeat(indent);
    write!(f, "{}", prefix)?;
    d.judgment.fmt_judgment(f, false)?;

    if d.premises.is_empty() {
        writeln!(f, " by {} {{}};", d.rule)
    } else {
        writeln!(f, " by {} {{", d.rule)?;
        for premise in &d.premises {
            fmt_with_indent(premise, f, indent + 1)?;
        }
        writeln!(f, "{}}};", prefix)
    }
}