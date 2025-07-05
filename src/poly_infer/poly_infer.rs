use std::collections::{BTreeMap, HashSet};
use crate::common::ast::{Expr, Op, Type, TypeVar, TyScheme, TypeEnv, Judgment};
use crate::poly_infer::proof::Derivation;
use crate::poly_infer::unifier::{unify, apply_sub, Substitution};

// The context for inference, holding substitutions.
struct InferContext {
    sub: Substitution,
    var_counter: usize,
    used_names: HashSet<String>,
}

impl InferContext {
    fn new_type_var(&mut self) -> Type {
        let mut name_id = 0;
        loop {
            let name = format!("'{}", ((name_id % 26) as u8 + b'a') as char);
            if !self.used_names.contains(&name) {
                self.used_names.insert(name.clone());
                let id = self.var_counter;
                self.var_counter += 1;
                // Use a large offset to ensure inferrer IDs do not collide with parser IDs.
                print!("{}", name);
                return Type::Var(TypeVar { id: id + 1000, name });
            }
            name_id += 1;
        }
    }
}

/// The main public entry point for the type inferrer.
/// It takes a judgment `Γ ⊢ e`, infers its type, and returns a full derivation proof.
pub fn infer_judgment(judgment: &Judgment, used_names: HashSet<String>) -> Result<Derivation, String> {
    match judgment {
        // Handle the case for polymorphic type inference (PolyTypingML4)
        Judgment::PolyInfer(env, expr, expected_ty) => {
            let mut ctx = InferContext {
                sub: BTreeMap::new(),
                var_counter: 0,
                used_names
            };
            // We infer the expression's type, with polymorphism enabled.
            let mut inferred_derivation = infer_expr(&mut ctx, env, expr)?;
            
            let final_sub = unify(&inferred_derivation.ty, expected_ty, &ctx.sub)?;
            ctx.sub = final_sub;

            apply_sub_to_deriv(&mut inferred_derivation, &ctx.sub);
            inferred_derivation.ty = apply_sub(expected_ty, &ctx.sub);
            Ok(inferred_derivation)
        }
        _ => Err("This judgment type is not supported by the type checker.".to_string()),
    }
}

/// The recursive helper that generates and solves type constraints,
/// while simultaneously building the derivation tree.
fn infer_expr(ctx: &mut InferContext, env: &TypeEnv, e: &Expr) -> Result<Derivation, String> {
    match e {
        Expr::Int(_) => Ok(Derivation {
            env: env.clone(), expr: e.clone(), ty: Type::Int,
            rule: "T-Int".to_string(), premises: vec![],
        }),
        Expr::Bool(_) => Ok(Derivation {
            env: env.clone(), expr: e.clone(), ty: Type::Bool,
            rule: "T-Bool".to_string(), premises: vec![],
        }),
        Expr::Nil => Ok(Derivation {
            env: env.clone(), expr: e.clone(), ty: Type::List(Box::new(ctx.new_type_var())),
            rule: "T-Nil".to_string(), premises: vec![],
        }),
        Expr::Var(var) => {
            for (v, scheme) in env.iter().rev() {
                if v == var {
                    // When using a variable, we instantiate its type scheme
                    // to get a fresh type with new type variables.
                    let ty = instantiate(scheme, ctx);
                    return Ok(Derivation {
                        env: env.clone(), expr: e.clone(), ty,
                        rule: "T-Var".to_string(), premises: vec![],
                    });
                }
            }
            Err(format!("Unbound variable: {}", var.0))
        }
        Expr::Fun(param, body, _) => {
            let param_ty = ctx.new_type_var();
            let mut new_env = env.clone();
            // In a `fun`, the parameter is monomorphic (not a `forall` type).
            new_env.push((param.clone(), TyScheme { vars: vec![], ty: param_ty.clone() }));
            
            let body_deriv = infer_expr(ctx, &new_env, body)?;
            
            let fun_ty = Type::Fun(Box::new(apply_sub(&param_ty, &ctx.sub)), Box::new(body_deriv.ty.clone()));
            
            Ok(Derivation {
                env: env.clone(), expr: e.clone(), ty: fun_ty,
                rule: "T-Abs".to_string(), premises: vec![body_deriv],
            })
        }
        Expr::App(e1, e2, _) => {
            let d1 = infer_expr(ctx, env, e1)?;
            let t1 = apply_sub(&d1.ty, &ctx.sub);
            let d2 = infer_expr(ctx, env, e2)?;
            let t2 = apply_sub(&d2.ty, &ctx.sub);

            let return_ty = ctx.new_type_var();
            let fun_ty = Type::Fun(Box::new(t2), Box::new(return_ty.clone()));
            
            ctx.sub = unify(&t1, &fun_ty, &ctx.sub)?;
            
            Ok(Derivation {
                env: env.clone(), expr: e.clone(), ty: return_ty,
                rule: "T-App".to_string(), premises: vec![d1, d2],
            })
        }
        Expr::Let(x, e1, e2, _) => {
            let d1 = infer_expr(ctx, env, e1)?;
            let t1 = apply_sub(&d1.ty, &ctx.sub);

            let mut new_env = env.clone();
            // (GEN) rule: generalize the type of the bound expression.
            let scheme = generalize(env, &t1, &ctx.sub);
            new_env.push((x.clone(), scheme));
            
            let d2 = infer_expr(ctx, &new_env, e2)?;
            
            Ok(Derivation {
                env: env.clone(), expr: e.clone(), ty: d2.ty.clone(),
                rule: "T-Let".to_string(), premises: vec![d1, d2],
            })
        }
        Expr::If(cond, then_branch, else_branch, _) => {
            let d_cond = infer_expr(ctx, env, cond)?;
            ctx.sub = unify(&d_cond.ty, &Type::Bool, &ctx.sub)?;
            let d_then = infer_expr(ctx, env, then_branch)?;
            let d_else = infer_expr(ctx, env, else_branch)?;
            ctx.sub = unify(&d_then.ty, &d_else.ty, &ctx.sub)?;
            Ok(Derivation {
                env: env.clone(), expr: e.clone(), ty: d_then.ty.clone(),
                rule: "T-If".to_string(), premises: vec![d_cond, d_then, d_else],
            })
        }
        Expr::LetRec(f, x, e1, e2, _) => {
            let t1 = ctx.new_type_var();
            let t2 = ctx.new_type_var();
            let fun_ty = Type::Fun(Box::new(t1.clone()), Box::new(t2.clone()));
            let mut new_env1 = env.clone();
            new_env1.push((f.clone(), TyScheme { vars: vec![], ty: fun_ty.clone() }));
            new_env1.push((x.clone(), TyScheme { vars: vec![], ty: t1 }));
            let d1 = infer_expr(ctx, &new_env1, e1)?;
            ctx.sub = unify(&d1.ty, &t2, &ctx.sub)?;
            let mut new_env2 = env.clone();
            new_env2.push((f.clone(), generalize(env, &fun_ty, &ctx.sub)));
            let d2 = infer_expr(ctx, &new_env2, e2)?;
            Ok(Derivation {
                env: env.clone(), expr: e.clone(), ty: d2.ty.clone(),
                rule: "T-LetRec".to_string(), premises: vec![d1, d2],
            })
        }
        Expr::BinOp(e1, op, e2, _) => {
            let d1 = infer_expr(ctx, env, e1)?;
            let t1 = apply_sub(&d1.ty, &ctx.sub);
            let d2 = infer_expr(ctx, env, e2)?;
            let t2 = apply_sub(&d2.ty, &ctx.sub);
            
            let (expected_t1, expected_t2, result_ty, rule_name) = match op {
                Op::Add => (Type::Int, Type::Int, Type::Int, "T-Plus"),
                Op::Sub => (Type::Int, Type::Int, Type::Int, "T-Minus"),
                Op::Mul => (Type::Int, Type::Int, Type::Int, "T-Mult"),
                Op::Lt => (Type::Int, Type::Int, Type::Bool, "T-Lt"),
                Op::Cons => {
                    let elem_type = ctx.new_type_var();
                    (elem_type.clone(), Type::List(Box::new(elem_type)), t2.clone(), "T-Cons")
                }
            };
            
            let sub1 = unify(&t1, &expected_t1, &ctx.sub)?;
            let sub2 = unify(&t2, &expected_t2, &sub1)?;
            ctx.sub = sub2;
            
            let final_type = apply_sub(&result_ty, &ctx.sub);
            Ok(Derivation {
                env: env.clone(),
                expr: e.clone(),
                ty: final_type,
                rule: rule_name.to_string(),
                premises: vec![d1, d2]
            })
        }
        Expr::Match(e1, e2, x, y, e3, _) => {
            let d1 = infer_expr(ctx, env, e1)?;
            let t1 = apply_sub(&d1.ty, &ctx.sub);
            let elem_ty = ctx.new_type_var();
            let list_ty = Type::List(Box::new(elem_ty.clone()));
            ctx.sub = unify(&t1, &list_ty, &ctx.sub)?;
            let d2 = infer_expr(ctx, env, e2)?;
            let t_nil = apply_sub(&d2.ty, &ctx.sub);
            let mut new_env = env.clone();
            new_env.push((x.clone(), TyScheme { vars: vec![], ty: elem_ty }));
            new_env.push((y.clone(), TyScheme { vars: vec![], ty: list_ty }));
            let d3 = infer_expr(ctx, &new_env, e3)?;
            let t_cons = apply_sub(&d3.ty, &ctx.sub);
            ctx.sub = unify(&t_nil, &t_cons, &ctx.sub)?;
            Ok(Derivation {
                env: env.clone(), expr: e.clone(), ty: t_nil,
                rule: "T-Match".to_string(), premises: vec![d1, d2, d3],
            })
        }
        _ => Err("This judgment type is not supported by the type checker.".to_string()),
    }
}

// --- Polymorphism and Finalization Helpers ---
fn generalize(env: &TypeEnv, ty: &Type, sub: &Substitution) -> TyScheme {
    let ty = apply_sub(ty, sub);
    let mut env_ftv = HashSet::new();
    for (_, scheme) in env {
        env_ftv.extend(apply_sub(&scheme.ty, sub).free_type_vars());
    }
    let ty_ftv = ty.free_type_vars();
    
    // Collect the difference into a Vec
    let mut quantified_vars: Vec<_> = ty_ftv.difference(&env_ftv).cloned().collect();
    
    // Sort the quantified variables by their ID to ensure a canonical order.
    // This is crucial for deterministic instantiation and display.
    quantified_vars.sort_by_key(|v| v.id);
    
    TyScheme { vars: quantified_vars, ty }
}

fn instantiate(scheme: &TyScheme, ctx: &mut InferContext) -> Type {
    let mut fresh_sub = Substitution::new();
    for var in &scheme.vars {
        fresh_sub.insert(var.clone(), ctx.new_type_var());
    }
    apply_sub(&scheme.ty, &fresh_sub)
}

fn apply_sub_to_env(env: &TypeEnv, sub: &Substitution) -> TypeEnv {
    env.iter()
        .map(|(var, scheme)| {
            // We must not substitute the variables that are quantified by this scheme.
            let mut temp_sub = sub.clone();
            for quantified_var in &scheme.vars {
                temp_sub.remove(quantified_var);
            }
            let new_ty = apply_sub(&scheme.ty, &temp_sub);
            (var.clone(), TyScheme { vars: scheme.vars.clone(), ty: new_ty })
        })
        .collect()
}

/// Applies substitutions to all types within a derivation tree.
fn apply_sub_to_deriv(deriv: &mut Derivation, sub: &Substitution) {
    deriv.ty = apply_sub(&deriv.ty, sub);
    deriv.env = apply_sub_to_env(&deriv.env, sub); // Use the corrected helper
    for premise in &mut deriv.premises {
        apply_sub_to_deriv(premise, sub);
    }
}
