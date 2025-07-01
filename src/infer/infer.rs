use std::collections::HashMap;
use crate::infer::ast::{Expr, Op, Var, Type, TypeVar, TypeEnv, Judgment};
use crate::infer::proof::Derivation;
use crate::infer::unification::{unify, apply_sub, Substitution};

// The context for inference, holding substitutions and the type environment.
struct InferContext {
    sub: Substitution,
    env: TypeEnv,
}

impl InferContext {
    /// Creates a new type variable and wraps it in a Type enum.
    fn new_type_var(&self) -> Type {
        Type::Var(TypeVar::new())
    }
}

/// The main public entry point for the type inferrer.
/// It takes a judgment `Γ ⊢ e`, infers its type, and returns a full derivation proof.
pub fn infer_judgment(judgment: &Judgment) -> Result<Derivation, String> {
    let mut ctx = InferContext {
        sub: HashMap::new(),
        env: judgment.env.clone(),
    };
    // 1. Recursively infer the type and build the initial derivation tree.
    let mut derivation = infer_expr(&mut ctx, &judgment.expr)?;

    // 2. Apply the final substitutions to the entire derivation tree.
    apply_sub_to_deriv(&mut derivation, &ctx.sub);

    Ok(derivation)
}

/// Helper function to apply substitutions to a type environment.
fn apply_sub_to_env(env: &TypeEnv, sub: &Substitution) -> TypeEnv {
    env.iter()
        .map(|(var, ty)| (var.clone(), apply_sub(ty, sub)))
        .collect()
}

/// Applies substitutions to all types within a derivation tree, including environments.
fn apply_sub_to_deriv(deriv: &mut Derivation, sub: &Substitution) {
    // Apply substitutions to the conclusion type of the current derivation step.
    deriv.ty = apply_sub(&deriv.ty, sub);
    // CORRECTED: Also apply substitutions to the environment of the current step.
    deriv.env = apply_sub_to_env(&deriv.env, sub);

    // Recursively apply to all premises.
    for premise in &mut deriv.premises {
        apply_sub_to_deriv(premise, sub);
    }
}


/// The recursive helper that generates and solves type constraints,
/// while simultaneously building the derivation tree.
fn infer_expr(ctx: &mut InferContext, e: &Expr) -> Result<Derivation, String> {
    match e {
        Expr::Int(_) => Ok(Derivation {
            env: ctx.env.clone(), expr: e.clone(), ty: Type::Int,
            rule: "T-Int".to_string(), premises: vec![],
        }),
        Expr::Bool(_) => Ok(Derivation {
            env: ctx.env.clone(), expr: e.clone(), ty: Type::Bool,
            rule: "T-Bool".to_string(), premises: vec![],
        }),
        Expr::Nil => {
            let elem_type = ctx.new_type_var();
            Ok(Derivation {
                env: ctx.env.clone(), expr: e.clone(), ty: Type::List(Box::new(elem_type)),
                rule: "T-Nil".to_string(), premises: vec![],
            })
        }
        Expr::Var(var) => {
            for (v, ty) in &ctx.env {
                if v == var {
                    return Ok(Derivation {
                        env: ctx.env.clone(), expr: e.clone(), ty: ty.clone(),
                        rule: "T-Var".to_string(), premises: vec![],
                    });
                }
            }
            Err(format!("Unbound variable: {}", var.0))
        }
        Expr::Fun(param, body, _) => {
            let param_ty = ctx.new_type_var();
            let mut new_env = ctx.env.clone();
            new_env.push((param.clone(), param_ty.clone()));
            
            let mut body_ctx = InferContext { sub: ctx.sub.clone(), env: new_env };
            let body_deriv = infer_expr(&mut body_ctx, body)?;
            
            ctx.sub = body_ctx.sub;
            let fun_ty = Type::Fun(Box::new(param_ty), Box::new(body_deriv.ty.clone()));
            
            Ok(Derivation {
                env: ctx.env.clone(), expr: e.clone(), ty: fun_ty,
                rule: "T-Fun".to_string(), premises: vec![body_deriv],
            })
        }
        Expr::App(e1, e2, _) => {
            let d1 = infer_expr(ctx, e1)?;
            let t1 = apply_sub(&d1.ty, &ctx.sub);

            let d2 = infer_expr(ctx, e2)?;
            let t2 = apply_sub(&d2.ty, &ctx.sub);

            let return_ty = ctx.new_type_var();
            let fun_ty = Type::Fun(Box::new(t2), Box::new(return_ty.clone()));
            
            let final_sub = unify(&t1, &fun_ty, &ctx.sub)?;
            ctx.sub = final_sub;
            
            let final_type = apply_sub(&return_ty, &ctx.sub);
            Ok(Derivation {
                env: ctx.env.clone(), expr: e.clone(), ty: final_type,
                rule: "T-App".to_string(), premises: vec![d1, d2],
            })
        }
        Expr::Let(x, e1, e2, _) => {
            let d1 = infer_expr(ctx, e1)?;
            let t1 = apply_sub(&d1.ty, &ctx.sub);

            let mut new_env = ctx.env.clone();
            new_env.push((x.clone(), t1));
            
            let mut body_ctx = InferContext { sub: ctx.sub.clone(), env: new_env };
            let d2 = infer_expr(&mut body_ctx, e2)?;
            
            ctx.sub = body_ctx.sub;
            let final_type = d2.ty.clone();
            
            Ok(Derivation {
                env: ctx.env.clone(), expr: e.clone(), ty: final_type,
                rule: "T-Let".to_string(), premises: vec![d1, d2],
            })
        }
        Expr::BinOp(e1, op, e2, _) => {
            let d1 = infer_expr(ctx, e1)?;
            let t1 = apply_sub(&d1.ty, &ctx.sub);

            let d2 = infer_expr(ctx, e2)?;
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
                env: ctx.env.clone(),
                expr: e.clone(),
                ty: final_type,
                rule: rule_name.to_string(),
                premises: vec![d1, d2]
            })
        }
        Expr::If(cond, then_branch, else_branch, _) => {
            let d_cond = infer_expr(ctx, cond)?;
            let t_cond = apply_sub(&d_cond.ty, &ctx.sub);
            ctx.sub = unify(&t_cond, &Type::Bool, &ctx.sub)?;
            
            let d_then = infer_expr(ctx, then_branch)?;
            let t_then = apply_sub(&d_then.ty, &ctx.sub);

            let d_else = infer_expr(ctx, else_branch)?;
            let t_else = apply_sub(&d_else.ty, &ctx.sub);

            ctx.sub = unify(&t_then, &t_else, &ctx.sub)?;
            let final_type = apply_sub(&t_then, &ctx.sub);

            Ok(Derivation {
                env: ctx.env.clone(), expr: e.clone(), ty: final_type,
                rule: "T-If".to_string(), premises: vec![d_cond, d_then, d_else],
            })
        }
        Expr::LetRec(f, x, e1, e2, _) => {
            let t1 = ctx.new_type_var();
            let t2 = ctx.new_type_var();
            let fun_ty = Type::Fun(Box::new(t1.clone()), Box::new(t2.clone()));
            
            let mut new_env1 = ctx.env.clone();
            new_env1.push((f.clone(), fun_ty.clone()));
            new_env1.push((x.clone(), t1));
            
            let mut body_ctx = InferContext { sub: ctx.sub.clone(), env: new_env1 };
            let d1 = infer_expr(&mut body_ctx, e1)?;
            let t_body = apply_sub(&d1.ty, &body_ctx.sub);
            
            ctx.sub = unify(&t_body, &t2, &body_ctx.sub)?;

            let mut new_env2 = ctx.env.clone();
            new_env2.push((f.clone(), fun_ty));
            
            let mut cont_ctx = InferContext { sub: ctx.sub.clone(), env: new_env2 };
            let d2 = infer_expr(&mut cont_ctx, e2)?;
            ctx.sub = cont_ctx.sub;
            
            let final_type = d2.ty.clone();
            Ok(Derivation {
                env: ctx.env.clone(), expr: e.clone(), ty: final_type,
                rule: "T-LetRec".to_string(), premises: vec![d1, d2],
            })
        }
        Expr::Match(e1, e2, x, y, e3, _) => {
            let d1 = infer_expr(ctx, e1)?;
            let t1 = apply_sub(&d1.ty, &ctx.sub);

            let elem_ty = ctx.new_type_var();
            let list_ty = Type::List(Box::new(elem_ty.clone()));
            ctx.sub = unify(&t1, &list_ty, &ctx.sub)?;

            let d2 = infer_expr(ctx, e2)?;
            let t_nil = apply_sub(&d2.ty, &ctx.sub);

            let mut new_env = ctx.env.clone();
            new_env.push((x.clone(), elem_ty.clone()));
            new_env.push((y.clone(), list_ty.clone()));
            let mut cons_ctx = InferContext { sub: ctx.sub.clone(), env: new_env };
            let d3 = infer_expr(&mut cons_ctx, e3)?;
            let t_cons = apply_sub(&d3.ty, &cons_ctx.sub);
            
            ctx.sub = unify(&t_nil, &t_cons, &cons_ctx.sub)?;
            let final_type = apply_sub(&t_nil, &ctx.sub);

            Ok(Derivation {
                env: ctx.env.clone(), expr: e.clone(), ty: final_type,
                rule: "T-Match".to_string(), premises: vec![d1, d2, d3],
            })
        }
    }
}
