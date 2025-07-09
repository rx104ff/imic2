use std::fmt;
use  crate::common::ast::{NamedExpr, PolyTypeEnv, Type};

#[derive(Debug)]
pub struct Derivation {
    pub env: PolyTypeEnv,
    pub expr: NamedExpr,
    pub ty: Type,
    pub rule: String,
    pub premises: Vec<Derivation>,
}

/// Helper function to format the type environment (gamma) for display.
fn format_env(env: &PolyTypeEnv) -> String {
    if env.is_empty() {
        // An empty environment is often omitted for clarity.
        "".to_string()
    } else {
        let binds = env
            .iter()
            .map(|(v, ty)| format!("{}: {}", v, ty))
            .collect::<Vec<_>>()
            .join(", ");
        // Add a space for formatting against the turnstile.
        format!("{} ", binds)
    }
}

/// Implements the pretty-printing logic for the derivation tree.
impl fmt::Display for Derivation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Inner recursive function to handle indentation.
        fn fmt_with_indent(d: &Derivation, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
            let prefix = "    ".repeat(indent);

            // The final output format will be: `env |- expr : type by T-RULE { ... }`
            writeln!(
                f,
                "{}{}|- {} : {} by {} {{",
                prefix,
                format_env(&d.env),
                d.expr, // Relies on the Display impl from type::ast
                d.ty,   // Relies on the Display impl from type::ast
                d.rule
            )?;

            // Recursively print all premises with increased indentation.
            for premise in &d.premises {
                fmt_with_indent(premise, f, indent + 1)?;
            }

            writeln!(f, "{}}};", prefix)
        }

        fmt_with_indent(self, f, 0)
    }
}
