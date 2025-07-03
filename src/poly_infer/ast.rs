use std::cell::{Cell, RefCell};
use std::fmt;
use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::hash::{Hash, Hasher};
use std::sync::Mutex;

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
// A thread-local state to manage the creation of all TypeVars for a single run.
// This ensures deterministic naming for tests.
struct TypeVarState {
    name_to_id: HashMap<String, usize>,
    next_id: usize,
}

impl TypeVarState {
    fn new() -> Self {
        TypeVarState { name_to_id: HashMap::new(), next_id: 0 }
    }
}

thread_local! {
    static TYPE_VAR_STATE: RefCell<TypeVarState> = RefCell::new(TypeVarState::new());
}

/// Resets the counter used for TypeVar IDs.
/// This MUST be called before each test run to ensure deterministic output.
pub fn reset_type_var_counter() {
    TYPE_VAR_STATE.with(|state| {
        *state.borrow_mut() = TypeVarState::new();
    });
}

impl TypeVar {
    /// Creates a fresh type variable for internal use by the inferrer.
    /// It generates the next available name ('a, 'b, 'c...) that hasn't been
    /// taken by a variable parsed from the source text.
    pub fn new() -> Self {
        TYPE_VAR_STATE.with(|state_cell| {
            let mut state = state_cell.borrow_mut();
            // Start searching for an unused name from the next available ID.
            let mut name_id_counter = state.next_id;
            loop {
                let name = format!("'{}", ((name_id_counter % 26) as u8 + b'a') as char);
                
                // Check if this generated name has already been parsed from the source.
                // We check the values of the map because the IDs are what matter.
                if !state.name_to_id.values().any(|&id| id == name_id_counter) {
                    // If the name is free, we can use it.
                    let id = state.next_id;
                    state.next_id += 1; // Consume the ID
                    
                    // We must also record this name as "used" so we don't generate it again.
                    state.name_to_id.insert(name.trim_start_matches('\'').to_string(), id);

                    return TypeVar { id, name };
                }
                
                // If the name was already taken (e.g., user wrote `'c` in the source,
                // which might have taken ID 2), try the next name in the sequence.
                name_id_counter += 1;
            }
        })
    }

    /// Creates a type variable from a name found in the source code (e.g. 'a).
    /// It ensures that the same name always maps to the same unique ID.
    pub fn new_from_name(name: String) -> Self {
        TYPE_VAR_STATE.with(|state_cell| {
            let mut state = state_cell.borrow_mut();
            // Check if we've already assigned an ID to this name.
            let id = if let Some(id) = state.name_to_id.get(&name) {
                *id
            } else {
                // If not, assign the next available ID and store it.
                let new_id = state.next_id;
                state.name_to_id.insert(name.clone(), new_id);
                state.next_id += 1;
                new_id
            };
            TypeVar { id, name: format!("'{}", name) }
        })
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
#[derive(Debug, Clone)]
pub struct TypeVar {
    pub id: usize, // Made public for hashing/eq
    name: String,
}

#[derive(Debug, Clone)]
pub struct TyScheme {
    pub vars: Vec<TypeVar>,
    pub ty: Type,
}

impl PartialEq for TyScheme {
    fn eq(&self, other: &Self) -> bool {
        if self.ty != other.ty { return false; }
        let self_vars: HashSet<_> = self.vars.iter().collect();
        let other_vars: HashSet<_> = other.vars.iter().collect();
        self_vars == other_vars
    }
}
impl Eq for TyScheme {}

impl PartialEq for TypeVar { fn eq(&self, other: &Self) -> bool { self.id == other.id } }
impl Eq for TypeVar {}
impl Hash for TypeVar { fn hash<H: Hasher>(&self, state: &mut H) { self.id.hash(state); } }

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
