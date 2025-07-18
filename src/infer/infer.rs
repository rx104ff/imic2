use std::collections::{BTreeMap};
use crate::common::ast::{MonoTypeEnv, NamedExpr};
use crate::common::ast::{Expr, Op, Type, TypeVar, Judgment};
use crate::infer::proof::Derivation;
use crate::common::unifier::{unify, apply_sub, Substitution};

// The context for inference, holding substitutions and the type environment.
struct InferContext {
    sub: Substitution,
    var_counter: usize,
}

impl InferContext {
    fn new_type_var(&mut self) -> Type {
        let name = format!("'{}", ((self.var_counter % 26) as u8 + b'a') as char);
        let id = self.var_counter;
        self.var_counter += 1;
        Type::Var(TypeVar { id: id + 1000, name })
    }
}

/// The main public entry point for the type system.
/// It takes a full judgment `Γ ⊢ e : τ` and kicks off the type checking process.
pub fn check_judgment(judgment: &Judgment) -> Result<Derivation, String> {
    match judgment {
        // Handle the case for monomorphic type checking (TypingML4)
        Judgment::Infer(env, expr, expected_ty) => {
            let mut ctx = InferContext {
                sub: BTreeMap::new(),
                var_counter: 0
            };
            // We start by inferring the expression's type, with polymorphism disabled.
            let mut inferred_derivation = check_expr(&mut ctx, env, expr, expected_ty)?;
            
            // Then, we unify the result with the expected type.
            
            apply_sub_to_deriv(&mut inferred_derivation, &ctx.sub);
            default_vars_in_deriv(&mut inferred_derivation);
            Ok(inferred_derivation)
        }
        // Add placeholders for other judgment types
        _ => Err("This judgment type is not supported by the type checker.".to_string()),
    }
}

/// The core recursive function of the type system.
/// It verifies that expression `e` has `expected_ty` in the current context.
fn check_expr(ctx: &mut InferContext, env: &MonoTypeEnv, e: &NamedExpr, expected_ty: &Type) -> Result<Derivation, String> {
    if let Expr::Fun(param, body, _) = e {
        if let Type::Fun(ty1, ty2) = expected_ty {
            let mut new_env = env.clone();
            new_env.push((param.clone(), *ty1.clone()));
            let mut body_ctx = InferContext {
                sub: ctx.sub.clone(),
                var_counter: ctx.var_counter
            };
            // Recursively check if the body has the expected return type.
            let premise = check_expr(&mut body_ctx, &new_env, body, ty2.as_ref())?;
            ctx.sub = body_ctx.sub; // Propagate substitutions
            return Ok(Derivation {
                env: env.clone(), expr: e.clone(), ty: expected_ty.clone(),
                rule: "T-Fun".to_string(), premises: vec![premise],
            });
        } else {
            return Err(format!("Type mismatch. Expression is a function, but expected type is {:?}.", expected_ty));
        }
    }

    // For all other expressions, we infer their type first...
    let mut inferred_derivation = infer_expr(ctx, env, e)?;
    
    // ...and then unify the inferred type with the one we expected.
    let final_sub = unify(&inferred_derivation.ty, expected_ty, &ctx.sub)?;
    ctx.sub = final_sub;

    // Update the derivation with the now more specific type.
    inferred_derivation.ty = apply_sub(expected_ty, &ctx.sub);
    Ok(inferred_derivation)
}

/// Helper function to apply substitutions to a type environment.
fn apply_sub_to_env(env: &MonoTypeEnv, sub: &Substitution) -> MonoTypeEnv {
    env.iter()
        .map(|(var, ty)| (var.clone(), apply_sub(ty, sub)))
        .collect()
}

/// Applies substitutions to all types within a derivation tree, including environments.
fn apply_sub_to_deriv(deriv: &mut Derivation, sub: &Substitution) {
    // Apply substitutions to the conclusion type of the current derivation step.
    deriv.ty = apply_sub(&deriv.ty, sub);
    deriv.env = apply_sub_to_env(&deriv.env, sub);

    // Recursively apply to all premises.
    for premise in &mut deriv.premises {
        apply_sub_to_deriv(premise, sub);
    }
}


/// The recursive helper that generates and solves type constraints,
/// while simultaneously building the derivation tree.
fn infer_expr(ctx: &mut InferContext, env: &MonoTypeEnv, e: &NamedExpr) -> Result<Derivation, String> {
    match e {
        Expr::Int(_) => Ok(Derivation {
            env: env.clone(), expr: e.clone(), ty: Type::Int,
            rule: "T-Int".to_string(), premises: vec![],
        }),
        Expr::Bool(_) => Ok(Derivation {
            env: env.clone(), expr: e.clone(), ty: Type::Bool,
            rule: "T-Bool".to_string(), premises: vec![],
        }),
        Expr::Nil => {
            let elem_type = ctx.new_type_var();
            Ok(Derivation {
                env: env.clone(), expr: e.clone(), ty: Type::List(Box::new(elem_type)),
                rule: "T-Nil".to_string(), premises: vec![],
            })
        }
        Expr::Var(var) => {
            for (v, ty) in env.iter().rev() { // CORRECTED: Iterate in reverse for correct scoping
                if v == var {
                    return Ok(Derivation {
                        env: env.clone(), expr: e.clone(), ty: ty.clone(),
                        rule: "T-Var".to_string(), premises: vec![],
                    });
                }
            }
            Err(format!("Unbound variable: {}", var.0))
        }
        Expr::Fun(param, body, _) => {
            let param_ty = ctx.new_type_var();
            let mut new_env = env.clone();
            new_env.push((param.clone(), param_ty.clone()));

            let body_deriv = infer_expr(ctx, &new_env, body)?;
            
            let fun_ty = Type::Fun(Box::new(param_ty), Box::new(body_deriv.ty.clone()));
            
            Ok(Derivation {
                env: env.clone(), expr: e.clone(), ty: fun_ty,
                rule: "T-Fun".to_string(), premises: vec![body_deriv],
            })
        }
        Expr::App(e1, e2, _) => {
            let d1 = infer_expr(ctx, env, e1)?;
            let t1 = apply_sub(&d1.ty, &ctx.sub);

            let d2 = infer_expr(ctx, env, e2)?;
            let t2 = apply_sub(&d2.ty, &ctx.sub);

            let return_ty = ctx.new_type_var();
            let fun_ty = Type::Fun(Box::new(t2), Box::new(return_ty.clone()));
            
            let final_sub = unify(&t1, &fun_ty, &ctx.sub)?;
            ctx.sub = final_sub;
            
            let final_type = apply_sub(&return_ty, &ctx.sub);
            Ok(Derivation {
                env: env.clone(), expr: e.clone(), ty: final_type,
                rule: "T-App".to_string(), premises: vec![d1, d2],
            })
        }
        Expr::Let(x, e1, e2, _) => {
            let d1 = infer_expr(ctx, env, e1)?;
            let t1 = apply_sub(&d1.ty, &ctx.sub);

            let mut new_env = env.clone();
            new_env.push((x.clone(), t1));
            
            let d2 = infer_expr(ctx, &new_env, e2)?;
            
            let final_type = d2.ty.clone();
            
            Ok(Derivation {
                env: env.clone(), expr: e.clone(), ty: final_type,
                rule: "T-Let".to_string(), premises: vec![d1, d2],
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
                Op::Mul => (Type::Int, Type::Int, Type::Int, "T-Times"),
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
        Expr::If(cond, then_branch, else_branch, _) => {
            let d_cond = infer_expr(ctx, env, cond)?;
            let t_cond = apply_sub(&d_cond.ty, &ctx.sub);
            ctx.sub = unify(&t_cond, &Type::Bool, &ctx.sub)?;
            
            let d_then = infer_expr(ctx, env, then_branch)?;
            let t_then = apply_sub(&d_then.ty, &ctx.sub);

            let d_else = infer_expr(ctx, env, else_branch)?;
            let t_else = apply_sub(&d_else.ty, &ctx.sub);

            ctx.sub = unify(&t_then, &t_else, &ctx.sub)?;
            let final_type = apply_sub(&t_then, &ctx.sub);

            Ok(Derivation {
                env: env.clone(), expr: e.clone(), ty: final_type,
                rule: "T-If".to_string(), premises: vec![d_cond, d_then, d_else],
            })
        }
        Expr::LetRec(f, x, e1, e2, _) => {
            let t1 = ctx.new_type_var();
            let t2 = ctx.new_type_var();
            let fun_ty = Type::Fun(Box::new(t1.clone()), Box::new(t2.clone()));
            
            let mut new_env1 = env.clone();
            new_env1.push((f.clone(), fun_ty.clone()));
            new_env1.push((x.clone(), t1));
            
            let d1 = infer_expr(ctx, &new_env1, e1)?;
            
            ctx.sub = unify(&d1.ty, &t2, &ctx.sub)?;

            let mut new_env2 = env.clone();
            new_env2.push((f.clone(), fun_ty));

            let d2 = infer_expr(ctx, &new_env2, e2)?;
            
            let final_type = d2.ty.clone();
            Ok(Derivation {
                env: env.clone(), expr: e.clone(), ty: final_type,
                rule: "T-LetRec".to_string(), premises: vec![d1, d2],
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
            new_env.push((x.clone(), elem_ty.clone()));
            new_env.push((y.clone(), list_ty.clone()));

            let d3 = infer_expr(ctx, &new_env, e3)?;
            let t_cons = apply_sub(&d3.ty, &ctx.sub);
            
            ctx.sub = unify(&t_nil, &t_cons, &ctx.sub)?;
            let final_type = apply_sub(&t_nil, &ctx.sub);

            Ok(Derivation {
                env: env.clone(), expr: e.clone(), ty: final_type,
                rule: "T-Match".to_string(), premises: vec![d1, d2, d3],
            })
            
        }
        _ => Err("This judgment type is not supported by the type checker.".to_string()),
    }
}

fn default_unconstrained_vars(t: &Type) -> Type {
    match t {
        Type::Var(_) => Type::Int, // Default hanging type variables to int
        Type::Fun(p, r) => Type::Fun(Box::new(default_unconstrained_vars(p)), Box::new(default_unconstrained_vars(r))),
        Type::List(inner) => Type::List(Box::new(default_unconstrained_vars(inner))),
        _ => t.clone(), // Concrete types (Int, Bool) remain unchanged.
    }
}

fn default_vars_in_deriv(deriv: &mut Derivation) {
    deriv.ty = default_unconstrained_vars(&deriv.ty);
    deriv.env = deriv.env.iter().map(|(v, t)| (v.clone(), default_unconstrained_vars(t))).collect();
    for premise in &mut deriv.premises {
        default_vars_in_deriv(premise);
    }
}
