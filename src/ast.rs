// src/ast.rs
#[derive(Debug, Clone)]
pub enum Expr {
    Number(i64),
    Var(String),
    Let(String, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Lambda(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    BinOp(String, Box<Expr>, Box<Expr>),
}
