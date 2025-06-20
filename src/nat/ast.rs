// The Mode enum is restored to select a specific, self-contained rule set.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComparisonMode {
    V1, // CompareNat1
    V2, // CompareNat2
    V3, // CompareNat3
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Nat {
    Z,
    S(Box<Nat>),
}

// Helper methods to perform Peano arithmetic.
// The derivator uses these to find intermediate values for premises.
impl Nat {
    pub fn plus(&self, other: &Nat) -> Nat {
        match self {
            Nat::Z => other.clone(),
            Nat::S(n) => Nat::S(Box::new(n.plus(other))),
        }
    }

    pub fn times(&self, other: &Nat) -> Nat {
        match self {
            Nat::Z => Nat::Z,
            Nat::S(n) => n.times(other).plus(other),
        }
    }

    pub fn is_less_than(&self, other: &Nat) -> bool {
        match other {
            Nat::Z => false, // Nothing is less than Z
            Nat::S(other_inner) => match self {
                Nat::Z => true, // Z is less than any S(n)
                Nat::S(self_inner) => self_inner.is_less_than(other_inner),
            },
        }
    }
}

// // Implement Display for pretty-printing the Nat values
// impl fmt::Display for Nat {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             Nat::Z => write!(f, "Z"),
//             Nat::S(n) => write!(f, "S({})", n),
//         }
//     }
// }

// An enum to represent the two types of arithmetic operations.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArithmeticOp {
    Plus,
    Times,
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operation {
    Plus,
    Times,
}

// The Judgment is now an enum to handle different statement forms.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Judgment {
    Arithmetic {
        op: ArithmeticOp,
        n1: Nat,
        n2: Nat,
        n3: Nat,
    },
    Comparison {
        n1: Nat,
        n2: Nat,
    },
}

// This Derivation struct is now defined here as you specified.
#[derive(Debug)]
pub struct Derivation {
    pub conclusion: String,
    pub rule: String,
    pub premises: Vec<Derivation>,
}