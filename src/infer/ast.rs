use std::fmt;
use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var(pub String);

impl From<String> for Var {
    fn from(s: String) -> Self {
        Var(s)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Lt,
    Cons,
}


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

// --- Types and Type Variables for Unification ---

// A unique identifier for a type variable.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct TypeVar(usize);

// A global atomic counter to ensure every type variable is unique.
static NEXT_TYPE_VAR_ID: AtomicUsize = AtomicUsize::new(0);

impl TypeVar {
    pub fn new() -> Self {
        TypeVar(NEXT_TYPE_VAR_ID.fetch_add(1, Ordering::SeqCst))
    }
}

// The Type enum now includes a variant for unknown types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
    Fun(Box<Type>, Box<Type>),
    List(Box<Type>),
    Var(TypeVar), // Represents an unknown type, e.g., 'a
}

pub type TypeEnv = Vec<(Var, Type)>;


#[derive(Debug, Clone, PartialEq)]
pub struct Judgment {
    pub env: TypeEnv,
    pub expr: Expr,
    pub ty: Type,
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Add => write!(f, "+"),
            Op::Sub => write!(f, "-"),
            Op::Mul => write!(f, "*"),
            Op::Lt => write!(f, "<"),
            Op::Cons => write!(f, "::"),
        }
    }
}

impl fmt::Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Display type variables like 'a, 'b, 'c...
        let c = ((self.0 % 26) as u8 + b'a') as char;
        if self.0 >= 26 {
            write!(f, "'{}{}", c, self.0 / 26)
        } else {
            write!(f, "'{}", c)
        }
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

impl fmt::Display for Judgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let env_str = self.env
            .iter()
            .map(|(v, t)| format!("{}: {}", v, t))
            .collect::<Vec<_>>()
            .join(", ");
        
        if env_str.is_empty() {
            write!(f, "|- {} : {}", self.expr, self.ty)
        } else {
            write!(f, "{} |- {} : {}", env_str, self.expr, self.ty)
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
