// src/common/ast/env.rs
use std::{fmt::{self, Display}, ops::{Deref, DerefMut, Index, IndexMut}};

use crate::common::ast::{core::{NamedVar, NamelessVar, Variable}, r#type::Type, value::Value};

/// A generic struct for an environment binding variables `V` to items `T`.
#[derive(Debug, Clone, PartialEq)]
pub struct Env<V: Variable, T>(pub Vec<(V, T)>);

impl<V: Variable, T> Env<V, T> {
    pub fn is_empty(&self) -> bool { self.0.is_empty() }
    pub fn iter(&self) -> std::slice::Iter<'_, (V, T)> { self.0.iter() }
    pub fn push(&mut self, item: (V, T)) { self.0.push(item); }
    pub fn len(&self) -> usize { self.0.len() }
}

impl<V: Variable, T> Index<usize> for Env<V, T> {
    type Output = (V, T);

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl<V: Variable, T> IndexMut<usize> for Env<V, T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.0[index]
    }
}

impl<V: Variable, T> Deref for Env<V, T> {
    type Target = Vec<(V, T)>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// This allows `Env` to be treated as a mutable slice `&mut [(V, T)]`
impl<V: Variable, T> DerefMut for Env<V, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<V: Variable, T> FromIterator<(V, T)> for Env<V, T> {
    fn from_iter<I: IntoIterator<Item = (V, T)>>(iter: I) -> Self {
        Env(iter.into_iter().collect())
    }
}

impl<'a, V: Variable, T> IntoIterator for &'a Env<V, T> {
    type Item = &'a (V, T);
    type IntoIter = std::slice::Iter<'a, (V, T)>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

pub type NamedEnv<T> = Env<NamedVar, T>;
pub type NamelessEnv<T> = Env<NamelessVar, T>;

// --- Display Traits & Helpers ---

/// Defines the binding separator (e.g., `=` or `:`) for an item in an environment.
pub trait EnvStyle {
    fn binding_separator(&self) -> &'static str;
}

impl<V: Variable> EnvStyle for Value<V> {
    fn binding_separator(&self) -> &'static str { "=" }
}

impl<V: Variable> EnvStyle for Type<V> {
    fn binding_separator(&self) -> &'static str { ":" }
}

/// A trait for displaying an item specifically within the context of an environment.
/// This is the key to breaking recursive display loops for `Value`.
pub trait EnvDisplay {
    fn fmt_for_env(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

/// A helper struct to invoke an `EnvDisplay` implementation within a `format!` macro.
pub struct AsEnvDisplay<'a, T: EnvDisplay + ?Sized>(&'a T);

impl<'a, T: EnvDisplay + ?Sized> Display for AsEnvDisplay<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt_for_env(f)
    }
}

/// Default implementation for `Type`, which is not recursive in a problematic way.
impl<V: Variable> EnvDisplay for Type<V> {
    fn fmt_for_env(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // `Type` can safely delegate to its standard `Display` implementation.
        write!(f, "{}", self)
    }
}

/// A generic trait to format a `(variable, item)` pair based on the variable type.
pub trait FormatStyleFor<V, T> {
    fn format_entry(v: &V, t: &T) -> String;
}

/// For named variables, format as `var = item` or `var : item`.
impl<T: EnvDisplay + EnvStyle> FormatStyleFor<NamedVar, T> for () {
    fn format_entry(v: &NamedVar, t: &T) -> String {
        format!("{}{}{}", v, t.binding_separator(), AsEnvDisplay(t))
    }
}

impl<T: EnvDisplay> FormatStyleFor<NamelessVar, T> for () {
    fn format_entry(_v: &NamelessVar, t: &T) -> String {
        format!("{}", AsEnvDisplay(t))
    }
}

impl<V, T> Display for Env<V, T>
where
    V: Variable,
    (): FormatStyleFor<V, T>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_empty() {
            return write!(f, "|- ");
        }
        let parts: Vec<String> = self.iter()
            .map(|(v, t)| <() as FormatStyleFor<_, _>>::format_entry(v, t))
            .collect();
        write!(f, "{} |- ", parts.join(", "))
    }
}
