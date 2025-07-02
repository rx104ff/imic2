use std::fmt;
use std::collections::HashSet;
use std::sync::atomic::{AtomicUsize, Ordering};

// --- Core Expression AST for PolyTypingML4 ---

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var(pub String);

#[derive(Debug, Clone, PartialEq)]
pub enum Op { Add, Sub, Mul, Lt, Cons }

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int(i64),
    Bool(bool),
    Var(Var),
    Let(Var, Box<Expr>, Box<Expr>, bool),
    LetRec(Var, Var, Box<Expr>, Box<Expr>, bool),
    Fun(Var, Box<Expr>, bool),
    App(Box<Expr>, Box<Expr>, bool),
    If(Box<Expr>, Box<Expr>, Box<Expr>, bool),
    BinOp(Box<Expr>, Op, Box<Expr>, bool),
    Nil,
    Match(Box<Expr>, Box<Expr>, Var, Var, Box<Expr>, bool),
}

// --- Types, Type Variables, and Type Schemes ---

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct TypeVar(usize);

static NEXT_TYPE_VAR_ID: AtomicUsize = AtomicUsize::new(0);

impl TypeVar {
    pub fn new() -> Self {
        TypeVar(NEXT_TYPE_VAR_ID.fetch_add(1, Ordering::SeqCst))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
    Fun(Box<Type>, Box<Type>),
    List(Box<Type>),
    Var(TypeVar), // Represents an unknown type, e.g., 'a
}

/// A Type Scheme represents a polymorphic type, e.g., `forall 'a. 'a -> 'a`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyScheme {
    pub vars: Vec<TypeVar>,
    pub ty: Type,
}

/// The Type Environment maps variables to polymorphic Type Schemes.
pub type TypeEnv = Vec<(Var, TyScheme)>;

/// The Judgment represents the input to the type inferrer: `Γ ⊢ e`.
#[derive(Debug, Clone, PartialEq)]
pub struct Judgment {
    pub env: TypeEnv,
    pub expr: Expr,
    pub ty: Type,
}

// --- Display Implementations for Pretty-Printing ---

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
        let c = ((self.0 % 26) as u8 + b'a') as char;
        if self.0 >= 26 { write!(f, "'{}{}", c, self.0 / 26) } 
        else { write!(f, "'{}", c) }
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
            let vars = self.vars.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(" ");
            write!(f, "{}. {}", vars, self.ty)
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
            Type::Var(tv) => { ftv.insert(*tv); }
            Type::Fun(t1, t2) => {
                t1.collect_ftv(ftv);
                t2.collect_ftv(ftv);
            }
            Type::List(t) => t.collect_ftv(ftv),
            _ => {}
        }
    }
}
