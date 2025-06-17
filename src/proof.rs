pub struct Derivation {
    pub env: String,
    pub expr: Expr,
    pub rule: String,
    pub result: String, // type or value
    pub children: Vec<Derivation>,
}
