use std::fmt;
use std::collections::{HashSet};
use std::hash::{Hash, Hasher};
use std::cmp::Ordering;

// --- Universal Primitives ---

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var(pub String);

#[derive(Debug, Clone, PartialEq)]
pub enum Op { Add, Sub, Mul, Lt, Cons }

// --- Types for Nat Language ---

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Nat {
    Z,
    S(Box<Nat>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReductionType {
    Single, // →
    Direct, // →d
    Multi,  // →*
}

// An enum to represent the two types of arithmetic operations.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArithmeticOp {
    Plus,
    Times,
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

// --- Value AST for the `eval` system ---
// These are the runtime values produced by the evaluator.

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Nil,
    Cons(Box<Value>, Box<Value>, bool),
    FunVal(Var, Box<Expr>, Vec<(Var, Value)>, bool),
    RecFunVal(Var, Var, Box<Expr>, Vec<(Var, Value)>, bool),
}

pub type Env = Vec<(Var, Value)>;

// --- Types for Type Systems (TypingML4 & PolyTypingML4) ---

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
    Fun(Box<Type>, Box<Type>),
    List(Box<Type>),
    Var(TypeVar),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyScheme {
    pub vars: Vec<TypeVar>,
    pub ty: Type,
}

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

pub type MonoTypeEnv = Vec<(Var, Type)>;
pub type PolyTypeEnv = Vec<(Var, TyScheme)>;

// --- Universal Expression AST ---

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    // Literals for ML, Nat, and Type systems
    Int(i64),
    Bool(bool),
    Nat(Nat),
    Var(Var),
    Nil,
    
    // ML & Type System Expressions
    Let(Var, Box<Expr>, Box<Expr>, bool),
    LetRec(Var, Var, Box<Expr>, Box<Expr>, bool),
    Fun(Var, Box<Expr>, bool),
    App(Box<Expr>, Box<Expr>, bool),
    If(Box<Expr>, Box<Expr>, Box<Expr>, bool),
    BinOp(Box<Expr>, Op, Box<Expr>, bool),
    Match(Box<Expr>, Box<Expr>, Var, Var, Box<Expr>, bool),
    
    // Nat Expression extensions
    Plus(Box<Expr>, Box<Expr>),
    Times(Box<Expr>, Box<Expr>),
}

// --- Universal Judgment AST ---

// The Judgment enum can now represent a judgment from ANY of your language systems.
#[derive(Debug, Clone, PartialEq)]
pub enum Judgment {
   
    // For Nat arithmetic
    Arithmetic { op: ArithmeticOp, n1: Nat, n2: Nat, n3: Nat },

    // For Nat comparison
    Comparison { n1: Nat, n2: Nat },

    // For Nat evaluation
    Evaluation { exp: Expr, n: Nat },

    // For Nat reduction
    Reduction { r_type: ReductionType, e1: Expr, e2: Expr },
    
    // For ML evaluation
    EvaluatesTo(Env, Expr), // Assuming Type can also represent ML values
    
    // For Type Checking
    Infer(MonoTypeEnv, Expr, Type),
    
    // For Polymorphic Inference
    PolyInfer(PolyTypeEnv, Expr, Type),
}

// --- All Display and Helper Implementations ---
impl fmt::Display for Nat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Nat::Z => write!(f, "Z"),
            Nat::S(n) => write!(f, "S({})", n),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(i) => write!(f, "{}", i),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "[]"),
            Value::Cons(h, t, is_paren) => {
                let s = format!("{} :: {}", h, t);
                if *is_paren { write!(f, "({})", s) } else { write!(f, "{}", s) }
            }
            Value::FunVal(param, body, env, _) => {
                let env_str = if env.is_empty() {
                    "()".to_string()
                } else {
                    format!("({})", env.iter()
                        .map(|(v, val)| format!("{} = {}", v, val))
                        .collect::<Vec<_>>().join(", "))
                };
                write!(f, "{}[fun {} -> {}]", env_str, param, body)
            }
            Value::RecFunVal(func, param, body, env, _) => {
                let env_str = if env.is_empty() {
                    "()".to_string()
                } else {
                    format!("({})", env.iter()
                        .map(|(v, val)| format!("{} = {}", v, val))
                        .collect::<Vec<_>>().join(", "))
                };
                write!(f, "{}[rec {} = fun {} -> {}]", env_str, func, param, body)
            }
        }
    }
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Add => write!(f, "+"), Op::Sub => write!(f, "-"), Op::Mul => write!(f, "*"),
            Op::Lt => write!(f, "<"), Op::Cons => write!(f, "::"),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Nat(n) => write!(f, "{}", n),
            Expr::Plus(e1, e2) => write!(f, "({} + {})", e1, e2),
            Expr::Times(e1, e2) => write!(f, "({} * {})", e1, e2),
            Expr::Int(n) => write!(f, "{}", n),
            Expr::Bool(b) => write!(f, "{}", b),
            Expr::Var(v) => write!(f, "{}", v.0),
            Expr::Nil => write!(f, "[]"),
            Expr::Let(x, e1, e2, is_paren) => {
                let s = format!("let {} = {} in {}", x, e1, e2);
                if *is_paren { write!(f, "({})", s) } else { write!(f, "{}", s) }
            },
            Expr::LetRec(func, param, body, cont, is_paren) => {
                let s = format!("let rec {} = fun {} -> {} in {}", func, param, body, cont);
                if *is_paren { write!(f, "({})", s) } else { write!(f, "{}", s) }
            },
            Expr::If(c, t, e, is_paren) => {
                let s = format!("if {} then {} else {}", c, t, e);
                if *is_paren { write!(f, "({})", s) } else { write!(f, "{}", s) }
            },
            Expr::BinOp(e1, op, e2, is_paren) => {
                let s = format!("{} {} {}", e1, op, e2);
                if *is_paren { write!(f, "({})", s) } else { write!(f, "{}", s) }
            }
            Expr::Fun(p, b, is_paren) => {
                let s = format!("fun {} -> {}", p, b);
                if *is_paren { write!(f, "({})", s) } else { write!(f, "{}", s) }
            },
            Expr::App(e1, e2, is_paren) => {
                let s = format!("{} {}", e1, e2);
                if *is_paren { write!(f, "({})", s) } else { write!(f, "{}", s) }
            },
            Expr::Match(e, nil_case, x, y, cons_case, is_paren) => {
                let s = format!("match {} with [] -> {} | {}::{} -> {}", e, nil_case, x, y, cons_case);
                if *is_paren { write!(f, "({})", s) } else { write!(f, "{}", s) }
            },
        }
    }
}

impl fmt::Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Bool => write!(f, "bool"),
            Type::Fun(t1, t2) => write!(f, "({} -> {})", t1, t2),
            Type::List(t) => write!(f, "{} list", t),
            Type::Var(tv) => write!(f, "{}", tv),
        }
    }
}

impl fmt::Display for TyScheme {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.vars.is_empty() {
            write!(f, "{}", self.ty)
        } else {
            let mut sorted_vars = self.vars.clone();
            // Sort by the string name to ensure 'a' comes before 'b'.
            sorted_vars.sort_by(|a, b| a.name.cmp(&b.name));

            let vars_str = sorted_vars.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(" ");
            write!(f, "{}. {}", vars_str, self.ty)
        }
    }
}

// --- Helper methods for finding free type variables ---
impl Type {
    pub fn free_type_vars(&self) -> HashSet<TypeVar> {
        let mut ftv = HashSet::new();
        self.collect_ftv(&mut ftv);
        ftv
    }

    fn collect_ftv(&self, ftv: &mut HashSet<TypeVar>) {
        match self {
            Type::Var(tv) => { ftv.insert(tv.clone()); }
            Type::Fun(t1, t2) => {
                t1.collect_ftv(ftv);
                t2.collect_ftv(ftv);
            }
            Type::List(t) => t.collect_ftv(ftv),
            _ => {}
        }
    }
}
