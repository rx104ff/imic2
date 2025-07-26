use std::fmt::{self, Debug};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReductionType {
    Single, // →
    Direct, // →d
    Multi,  // →*
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArithmeticOp {
    Plus,
    Times,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Op { Add, Sub, Mul, Lt, Cons, App, Fun }

impl Op {
    pub fn is_right_assoc(&self) -> bool {
        matches!(self, Op::Fun | Op::Cons)
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

pub trait Variable: std::fmt::Display + Clone + PartialEq + Sized + Debug {
    type Binder: std::fmt::Display + Clone + PartialEq + Debug;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DBIndex(pub usize);

impl fmt::Display for DBIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0 == 0 {
            write!(f, ".")
        } else {
            write!(f, "#{}", self.0)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NamedVar(pub String);
impl Variable for NamedVar {
    type Binder = NamedVar;
}

impl fmt::Display for NamedVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NamelessVar(pub DBIndex);
impl Variable for NamelessVar {
    type Binder = NamelessVar;
}

impl fmt::Display for NamelessVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
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

pub trait FromVar<V: Variable> {
    fn from_var(var: V) -> Self;
}

pub trait FromUnaryOp {
    fn from_unary_op(op: Op, operand: Self) -> Result<Self, String> where Self: Sized;
}

pub trait FromGroup {
    fn from_group(inner: Self) -> Self where Self: Sized;
}
