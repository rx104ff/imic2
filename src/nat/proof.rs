// A structure to represent a formal proof in the Nat system
#[derive(Debug)]
pub struct Derivation {
    pub conclusion: String,
    pub rule: String,
    pub premises: Vec<Derivation>,
}

