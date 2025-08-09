use crate::common::ast::core::{FromGroup, NamedVar, Op, Variable};
use crate::common::ast::env::Env;
use std::cmp::Ordering;
use std::collections::HashSet;
use std::hash::{Hash, Hasher};
use std::fmt::{self, Debug};

#[derive(Debug, Clone, PartialEq)]
pub enum Type<V: Variable> {
    Int,
    Bool,
    SS(V),
    Fun(Box<Type<V>>, Box<Type<V>>),
    List(Box<Type<V>>),
    Var(TypeVar),
    Group(Box<Type<V>>),
    BinOp(Box<Type<V>>, Op, Box<Type<V>>),
    Scheme(Box<Vec<TypeVar>>, Box<Type<V>>)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scheme<V: Variable>(pub Type<V>);

#[derive(Debug, Clone)]
pub struct TypeVar {
    pub id: usize,
    pub name: String,
}

impl PartialEq for TypeVar { 
    fn eq(&self, other: &Self) -> bool { 
        self.id == other.id 
    } 
}

impl Eq for TypeVar {}

impl PartialOrd for TypeVar { 
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> { 
        Some(self.cmp(other)) 
    } 
}

impl Ord for TypeVar { 
    fn cmp(&self, other: &Self) -> Ordering { 
        self.id.cmp(&other.id) 
    } 
}

impl Hash for TypeVar { 
    fn hash<H: Hasher>(&self, state: &mut H) { 
        self.id.hash(state); 
    } 
}

impl fmt::Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "'{}", self.name)
    }
}

impl<V: Variable> fmt::Display for Type<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Bool => write!(f, "bool"),
            Type::Fun(t1, t2) => write!(f, "({} -> {})", t1, t2),
            Type::Var(tv) => write!(f, "{}", tv),
            Type::List(t) => {
                if let Type::BinOp(_, _, _) = &**t {
                    write!(f, "({}) list", t)
                } else {
                    write!(f, "{} list", t)
                }
            },
            Type::Group(t) => write!(f, "({})", t),
            Type::BinOp(t1, op, t2) => {
                if op.is_right_assoc() {
                    if let Type::BinOp(_, _, _) = &**t1 {
                        write!(f, "({}) {} {}", t1, op, t2)
                    } else {
                        write!(f, "{} {} {}", t1, op, t2)
                    }
                } else {
                    if let Type::BinOp(_, _, _) = &**t2 {
                        write!(f, "{} {} ({})", t1, op, t2)
                    } else {
                        write!(f, "{} {} {}", t1, op, t2)
                    }
                }
            }
            Type::Scheme(vars, types ) => {
                if vars.is_empty() {
                    write!(f, "{}", types)
                } else {
                    let mut sorted_vars = vars.clone();
                    sorted_vars.sort_by(|a, b| a.name.cmp(&b.name));

                    let vars_str = sorted_vars.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(" ");
                    write!(f, "{}. {}", vars_str, types)
                        }
            }
            _ => write!(f, "{}", ""),
        }
    }
}

impl<V: Variable> Type<V> {
    pub fn free_type_vars(&self) -> HashSet<TypeVar> {
        let mut ftv = HashSet::new();
        self.collect_ftv(&mut ftv);
        ftv
    }

    fn collect_ftv(&self, ftv: &mut HashSet<TypeVar>) {
        match self {
            Type::Var(tv) => {
                ftv.insert(tv.clone());
            }
            Type::Fun(t1, t2) | Type::BinOp(t1, _, t2) => {
                t1.collect_ftv(ftv);
                t2.collect_ftv(ftv);
            }
            Type::List(t) | Type::Group(t) => {
                t.collect_ftv(ftv);
            }
            Type::Scheme(quantified_vars, inner_ty) => {
                // This is the most important rule:
                // First, find all free variables in the inner type.
                let mut inner_free_vars = inner_ty.free_type_vars();
                // Then, subtract the variables that are bound by this scheme's quantifier.
                for var in &**quantified_vars {
                    inner_free_vars.remove(var);
                }
                // Add the remaining free variables to the final set.
                ftv.extend(inner_free_vars);
            }

            Type::Int | Type::Bool | Type::SS(_) => {}
        }
    }
}

pub type MonoTypeEnv = Env<NamedVar, Type<NamedVar>>;

pub type PolyTypeEnv = Env<NamedVar, Type<NamedVar>>;

impl<V: Variable> FromGroup for Type<V> {
    fn from_group(inner: Self) -> Self {
        Type::Group(Box::new(inner))
    }
}
