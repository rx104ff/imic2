use crate::common::proof::Derivation;

/// The central trait for a derivable system.
pub trait System<J, Ctx> {
    /// Recursively derives a proof for a given judgment.
    fn derive(&self, ctx: &mut Ctx, judgment: &J) -> Result<Derivation<J>, String>;
}

/// A trait for a component that can provide an axiomatic (terminal) derivation.
pub trait Axiom<J, Ctx> {
    /// If this component can provide an axiom for the given judgment,
    /// it returns a terminal Derivation (which has no premises of its own).
    /// Returns `None` if this axiom provider doesn't apply to the judgment.
    fn axiom(&self, ctx: &mut Ctx, judgment: &J) -> Option<Result<Derivation<J>, String>>;
}

/// The generic trait for a component that provides a single, recursive derivation rule.
pub trait Rule<J, Ctx> {
    /// Attempts to apply the rule to a given judgment.
    ///
    /// If the rule's structure matches, it returns:
    /// 1. `String`: The name of the main rule (e.g., "E-Plus").
    /// 2. `Vec<J>`: A list of judgments that must be proven recursively.
    ///
    /// The `Result` handles any errors that might occur within the rule's logic.
    /// Returns `None` if the rule's structure does not match.
    fn apply(&self, ctx: &mut Ctx, judgment: &J) -> Option<Result<(String, Vec<J>), String>>;
}
