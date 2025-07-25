use std::fmt::{self, write, Debug};
use std::collections::{HashSet};
use std::hash::{Hash, Hasher};
use std::cmp::Ordering;

pub trait Variable: std::fmt::Display + Clone + PartialEq + Sized + Debug {
    type Binder: std::fmt::Display + Clone + PartialEq + Debug;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NamedVar(pub String);
impl Variable for NamedVar {
    type Binder = NamedVar;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NamelessVar(pub DBIndex);
impl Variable for NamelessVar {
    type Binder = NamelessVar;
}

impl fmt::Display for NamedVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for NamelessVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for DBIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0 == 0 {
            write!(f, ".")
        } else {
            write!(f, "#{}", self.0)
        }
    }
}

// --- Universal Primitives ---
#[derive(Debug, Clone, PartialEq)]
pub enum Op { Add, Sub, Mul, Lt, Cons, App, Fun }

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


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DBIndex(pub usize);

pub type NamedExpr = Expr<NamedVar>;
pub type NamelessExpr = Expr<NamelessVar>;

pub type NamedValue = Value<NamedVar>;
pub type NamelessValue = Value<NamelessVar>;

pub type NamedEnv = Env<NamedVar>;
pub type NamelessEnv = Env<NamelessVar>;

// --- Define a new, local trait for displaying environments ---
pub trait DisplayEnv {
    fn display_env(&self) -> String;
}

impl DisplayEnv for NamedEnv {
    fn display_env(&self) -> String {
        if self.is_empty() {
            format!("()")
        } else {
            let parts: Vec<String> = self.iter().map(|(v, val)| format!("{} = {}", v, val)).collect();
            format!("({})", parts.join(", "))
        }
    }
}

impl DisplayEnv for NamelessEnv {
    fn display_env(&self) -> String {
        if self.is_empty() {
            format!("()")
        } else {
            let parts: Vec<String> = self.iter().map(|(_, val)| format!("{}", val)).collect();
            format!("({})", parts.join(", "))
        }
    }
}

// --- Types for Type Systems (TypingML4 & PolyTypingML4) ---

#[derive(Debug, Clone, PartialEq)]
pub enum Type<V: Variable> {
    Int,
    Bool,
    SS(V),
    Fun(Box<Type<V>>, Box<Type<V>>),
    List(Box<Type<V>>),
    Var(TypeVar),
    Group(Box<Type<V>>),
    BinOp(Box<Type<V>>, Op, Box<Type<V>>)
}

#[derive(Debug, Clone, PartialEq)]
pub struct TyScheme<V: Variable> {
    pub vars: Vec<TypeVar>,
    pub ty: Type<V>,
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

impl Op {
    pub fn is_right_assoc(&self) -> bool {
        matches!(self, Op::Fun | Op::Cons)
    }
}

pub type MonoTypeEnv = Vec<(NamedVar, Type<NamedVar>)>;
pub type PolyTypeEnv = Vec<(NamedVar, TyScheme<NamedVar>)>;

// --- Universal Expression AST ---

pub type Env<V> = Vec<(V, Value<V>)>;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<V: Variable> {
    Int(i64),
    Bool(bool),
    Nat(Nat),
    Var(V),
    Nil,
    Let(V, Box<Expr<V>>, Box<Expr<V>>),
    LetRec(V, V, Box<Expr<V>>, Box<Expr<V>>),
    Fun(V, Box<Expr<V>>),
    App(Box<Expr<V>>, Box<Expr<V>>),
    If(Box<Expr<V>>, Box<Expr<V>>, Box<Expr<V>>),
    UnaryOp(Op, Box<Expr<V>>),
    BinOp(Box<Expr<V>>, Op, Box<Expr<V>>),
    Match(Box<Expr<V>>, Box<Expr<V>>, V, V, Box<Expr<V>>),
    Group(Box<Expr<V>>),
    Plus(Box<Expr<V>>, Box<Expr<V>>),
    Times(Box<Expr<V>>, Box<Expr<V>>),
}

impl<V: Variable> Expr<V> {
    pub fn into_variable(self) -> Option<V> {
        if let Expr::Var(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_variable(&self) -> Option<&V> {
        if let Expr::Var(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value<V: Variable> {
    Int(i64),
    Bool(bool),
    Nil,
    Cons(Box<Value<V>>, Box<Value<V>>),
    FunVal(V, Box<Expr<V>>, Env<V>),
    RecFunVal(V, V, Box<Expr<V>>, Env<V>),
    Group(Box<Value<V>>)
}


pub trait FromInt {
    fn from_int(n: i64) -> Self;
}

pub trait FromBool {
    fn from_bool(b: bool) -> Self;
}

pub trait FromNil {
    fn from_nil() -> Self;
}

// pub trait FromIntType {
//     fn from_int_type() -> Self;
// }

// pub trait FromIntType {
//     fn from_int_type() -> Self;
// }

pub trait FromVar<V: Variable> {
    fn from_var(var: V) -> Self;
}

pub trait FromUnaryOp {
    fn from_unary_op(op: Op, operand: Self) -> Result<Self, String> where Self: Sized;
}

pub trait FromGroup {
    fn from_group(inner: Self) -> Self where Self: Sized;
}

impl<V: Variable> FromInt for Expr<V> {
    fn from_int(n: i64) -> Self { Expr::Int(n) }
}

impl<V: Variable> FromBool for Expr<V> {
    fn from_bool(b: bool) -> Self { Expr::Bool(b) }
}

impl<V: Variable> FromNil for Expr<V> {
    fn from_nil() -> Self { Expr::Nil }
}

impl<V: Variable> FromVar<V> for Expr<V> {
    fn from_var(var: V) -> Self {
        Expr::Var(var)
    }
}

impl<V: Variable> FromInt for Value<V> {
    fn from_int(n: i64) -> Self { Value::Int(n) }
}

impl<V: Variable> FromBool for Value<V> {
    fn from_bool(b: bool) -> Self { Value::Bool(b) }
}

impl<V: Variable> FromNil for Value<V> {
    fn from_nil() -> Self { Value::Nil }
}

impl<V: Variable> FromUnaryOp for Expr<V> {
    fn from_unary_op(op: Op, operand: Self) -> Result<Self, String> {
        Ok(Expr::UnaryOp(op, Box::new(operand)))
    }
}

impl<V: Variable> FromUnaryOp for Value<V> {
    fn from_unary_op(op: Op, operand: Self) -> Result<Self, String> {
        if op == Op::Sub {
            if let Value::Int(i) = operand {
                return Ok(Value::Int(-i));
            }
        }
        Err(format!("Cannot apply unary operator {:?} to value {:?}", op, operand))
    }
}

impl<V: Variable> FromGroup for Expr<V> {
    fn from_group(inner: Self) -> Self {
        Expr::Group(Box::new(inner))
    }
}

impl<V: Variable> FromGroup for Value<V> {
    fn from_group(inner: Self) -> Self {
        Value::Group(Box::new(inner))
    }
}

impl<V: Variable> FromGroup for Type<V> {
    fn from_group(inner: Self) -> Self {
        Type::Group(Box::new(inner))
    }
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
    Evaluation { exp: Expr<NamedVar>, n: Nat },

    // For Nat reduction
    Reduction { r_type: ReductionType, e1: NamedExpr, e2: NamedExpr },

    NamelessEvaluation (NamelessEnv, NamelessExpr),
    
    // For ML evaluation
    EvaluatesTo(NamedEnv, NamedExpr), // Assuming Type can also represent ML values
    
    // For Type Checking
    Infer(MonoTypeEnv, NamedExpr, Type<NamedVar>),
    
    // For Polymorphic Inference
    PolyInfer(PolyTypeEnv, NamedExpr, Type<NamedVar>),
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

impl<E: Variable  + 'static> fmt::Display for Value<E> where E: std::fmt::Display,
Env<E>: DisplayEnv, // Add this trait bound
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(i) => write!(f, "{}", i),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "[]"),
            Value::Cons(h, t) => {
                let s = format!("{} :: {}", h, t);
                write!(f, "{}", s)
            }
            Value::FunVal(param, body, env) => {
                write!(f, "{}[fun {} -> {}]", env.display_env(), param, body)
            }
            Value::RecFunVal(func, param, body, env) => {
                write!(f, "{}[rec {} = fun {} -> {}]", env.display_env(), func, param, body)
            }
            Value::Group(v) => {
                write!(f, "({})", v)
            }
        }
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Add => write!(f, "+"), Op::Sub => write!(f, "-"), Op::Mul => write!(f, "*"),
            Op::Lt => write!(f, "<"), Op::Cons => write!(f, "::"), Op::App => write!(f, ""), Op::Fun => write!(f, "->")
        }
    }
}

impl<E> fmt::Display for Expr<E> where E: std::fmt::Display + Variable{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Nat(n) => write!(f, "{}", n),
            Expr::Plus(e1, e2) => write!(f, "({} + {})", e1, e2),
            Expr::Times(e1, e2) => write!(f, "({} * {})", e1, e2),
            Expr::Int(n) => write!(f, "{}", n),
            Expr::Bool(b) => write!(f, "{}", b),
            Expr::Var(v) => write!(f, "{}", v),
            Expr::Nil => write!(f, "[]"),
            Expr::Let(x, e1, e2) => {
                let s = format!("let {} = {} in {}", x, e1, e2);
                write!(f, "{}", s)
            },
            Expr::LetRec(func, param, body, cont) => {
                let s = format!("let rec {} = fun {} -> {} in {}", func, param, body, cont);
                write!(f, "{}", s)
            },
            Expr::If(c, t, e) => {
                let s = format!("if {} then {} else {}", c, t, e);
                write!(f, "{}", s)
            },
            Expr::UnaryOp(op, e) => {
                let s = format!("{}{}", op, e);
               write!(f, "{}", s)
            }
            Expr::BinOp(e1, op, e2) => {
                let s = if *op == Op::App {
                    format!("{} {}", e1, e2)
                } else {
                    format!("{} {} {}", e1, op, e2)
                };
                write!(f, "{}", s)
            }
            Expr::Fun(p, b) => {
                let s = format!("fun {} -> {}", p, b);
                write!(f, "{}", s)
            },
            Expr::App(e1, e2) => {
                let s = format!("{} {}", e1, e2);
                write!(f, "{}", s)
            },
            Expr::Match(e, nil_case, x, y, cons_case) => {
                let s = format!("match {} with [] -> {} | {}::{} -> {}", e, nil_case, x, y, cons_case);
                write!(f, "{}", s)
            },
            Expr::Group(e) => {
                let s = format!("({})", e);
                write!(f, "{}", s)
            }
        }
    }
}

impl fmt::Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
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
                // Add parentheses if the inner type is a binary operation to avoid ambiguity.
                if let Type::BinOp(_, _, _) = &**t {
                    write!(f, "({}) list", t)
                } else {
                    write!(f, "{} list", t)
                }
            },
            Type::Group(t) => write!(f, "({})", t),
            Type::BinOp(t1, op, t2) => {
                // For a right-associative operator (like `->` or `::`), if the
                // left-hand side is also a binary operator, it needs parentheses
                // to preserve the correct grouping, e.g., `(A -> B) -> C`.
                if op.is_right_assoc() {
                    if let Type::BinOp(_, _, _) = &**t1 {
                        write!(f, "({}) {} {}", t1, op, t2)
                    } else {
                        write!(f, "{} {} {}", t1, op, t2)
                    }
                } else {
                    // For a left-associative operator (like `+`), if the
                    // right-hand side is also a binary operator, it needs parentheses.
                    if let Type::BinOp(_, _, _) = &**t2 {
                        write!(f, "{} {} ({})", t1, op, t2)
                    } else {
                        write!(f, "{} {} {}", t1, op, t2)
                    }
                }
            }
            _ => write!(f, "{}", ""),
        }
    }
}

impl<V: Variable> fmt::Display for TyScheme<V> {
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
impl<V: Variable> Type<V> {
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
