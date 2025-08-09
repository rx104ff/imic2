// The Mode enum is restored to select a specific, self-contained rule set.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComparisonMode {
    V1, // CompareNat1
    V2, // CompareNat2
    V3, // CompareNat3
}
