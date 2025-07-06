// src/ast.rs
use std::hash::Hash;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LanguageVersion {
    ML1,
    ML2,
    ML3,
    ML4,
}
