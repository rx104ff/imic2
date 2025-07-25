use std::collections::BTreeMap;
use crate::common::ast::{NamedVar, Type, TypeVar};

// A substitution map from a type variable to its inferred type, using BTreeMap for deterministic iteration.
pub type Substitution = BTreeMap<TypeVar, Type<NamedVar>>;

/// The main unification function. Tries to make two types equal.
pub fn unify(t1: &Type<NamedVar>, t2: &Type<NamedVar>, sub: &Substitution) -> Result<Substitution, String> {
    let t1 = apply_sub(t1, sub);
    let t2 = apply_sub(t2, sub);

    match (&t1, &t2) {
        // --- Group Unification ---
        // If one or both types are grouped, unify the inner types.
        (Type::Group(t1_inner), _) => unify(t1_inner, &t2, sub),
        (_, Type::Group(t2_inner)) => unify(&t1, t2_inner, sub),

        (Type::Var(tv1), Type::Var(tv2)) if tv1 == tv2 => Ok(sub.clone()),
        (Type::Var(tv), t) | (t, Type::Var(tv)) => unify_variable(tv, t, sub),
        (Type::Int, Type::Int) => Ok(sub.clone()),
        (Type::Bool, Type::Bool) => Ok(sub.clone()),
        (Type::BinOp(p1, op1, r1), Type::BinOp(p2, op2, r2)) => {
            if op1 == op2 {
                let sub1 = unify(p1.as_ref(), p2.as_ref(), sub)?;
                let sub2 = unify(r1.as_ref(), r2.as_ref(), &sub1)?;
                Ok(sub2)
            } else {
                Err("Type mismatch: operators in BinOp do not match".to_owned())
            }
        }
        (Type::List(t1_inner), Type::List(t2_inner)) => {
            unify(t1_inner.as_ref(), t2_inner.as_ref(), sub)
        }
        (t1, t2) => Err(format!("Type mismatch: cannot unify {:?} with {:?}", t1, t2)),
    }
}

/// Unifies a variable with a type, performing the crucial "occurs check".
fn unify_variable(tv: &TypeVar, t: &Type<NamedVar>, sub: &Substitution) -> Result<Substitution, String> {
    // If the variable is already in the substitution, we work with its concrete type.
    if let Type::Var(tv2) = t {
        if tv.id == tv2.id {
            return Ok(sub.clone());
        }
        // This makes the unification process deterministic.
        let (from, to) = if tv.id > tv2.id { (tv, tv2) } else { (tv2, tv) };
        let mut new_sub = sub.clone();
        new_sub.insert(from.clone(), Type::Var(to.clone()));
        return Ok(new_sub);
    }
    
    if occurs(tv, t, sub) {
        return Err(format!("Recursive type detected: {} occurs in {}", tv, t));
    }
    let mut new_sub = sub.clone();
    new_sub.insert(tv.clone(), t.clone());
    Ok(new_sub)
}

/// Applies substitutions to a type to get its most current form.
pub fn apply_sub(t: &Type<NamedVar>, sub: &Substitution) -> Type<NamedVar> {
    match t {
        Type::Var(tv) => sub.get(tv).map_or(t.clone(), |st| apply_sub(st, sub)),
        Type::Fun(p, r) => Type::Fun(Box::new(apply_sub(p, sub)), Box::new(apply_sub(r, sub))),
        Type::BinOp(p, op, r) => Type::BinOp(
            Box::new(apply_sub(p, sub)),
            op.clone(),
            Box::new(apply_sub(r, sub)),
        ),
        Type::List(t) => Type::List(Box::new(apply_sub(t, sub))),
        Type::Group(t_inner) => Type::Group(Box::new(apply_sub(t_inner, sub))),
        _ => t.clone(),
    }
}

/// Checks if a type variable occurs within a type (to prevent infinite types).
fn occurs(tv: &TypeVar, t: &Type<NamedVar>, sub: &Substitution) -> bool {
    match t {
        Type::Var(tv2) => {
            if tv == tv2 { return true; }
            if let Some(st) = sub.get(tv2) {
                occurs(tv, st, sub)
            } else { false }
        }
        Type::Fun(p, r) => occurs(tv, p, sub) || occurs(tv, r, sub),
        Type::BinOp(p, _, r) => occurs(tv, p, sub) || occurs(tv, r, sub),
        Type::List(t) => occurs(tv, t, sub),
        Type::Group(t_inner) => occurs(tv, t_inner, sub),
        _ => false,
    }
}
