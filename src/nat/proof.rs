// A structure to represent a formal proof in the Nat system
#[derive(Debug)]
pub struct Derivation {
    pub conclusion: String, // e.g., "S(Z) plus Z is S(Z)"
    pub rule: String,       // e.g., "P-SUCC"
    pub premises: Vec<Derivation>, // The sub-proofs this step depends on
}

