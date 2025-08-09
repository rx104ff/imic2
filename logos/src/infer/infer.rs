// src/infer/infer.rs
use std::collections::{BTreeMap};
use crate::common::ast::core::{NamedVar, Op};
use crate::common::ast::expr::{Expr, NamedExpr};
use crate::common::ast::judgement::Judgment as GlobalJudgment; // Alias to avoid conflict
use crate::common::ast::r#type::{MonoTypeEnv, Type, TypeVar};
// Import the generic Derivation and Judgment
use crate::common::proof::{Derivation, Judgment};
use crate::common::unifier::{unify, apply_sub, Substitution};

// The context for inference, holding substitutions and the type environment.
struct InferContext {
    sub: Substitution,
    var_counter: usize,
}

impl InferContext {
    fn new_type_var(&mut self) -> Type<NamedVar> {
        let name = format!("'{}", ((self.var_counter % 26) as u8 + b'a') as char);
        let id = self.var_counter;
        self.var_counter += 1;
        Type::Var(TypeVar { id: id + 1000, name })
    }
}

/// The main public entry point for the type system.
pub fn check_judgment(judgment: &GlobalJudgment) -> Result<Derivation<Judgment<NamedVar, Type<NamedVar>, Type<NamedVar>>>, String> {
    match judgment {
        GlobalJudgment::Infer(env, expr, expected_ty) => {
            let mut ctx = InferContext {
                sub: BTreeMap::new(),
                var_counter: 0
            };
            let mut inferred_derivation = check_expr(&mut ctx, env, expr, expected_ty)?;
            
            apply_sub_to_deriv(&mut inferred_derivation, &ctx.sub);
            default_vars_in_deriv(&mut inferred_derivation);
            Ok(inferred_derivation)
        }
        _ => Err("This judgment type is not supported by the type checker.".to_string()),
    }
}

/// The core recursive function of the type system.
fn check_expr(
    ctx: &mut InferContext,
    env: &MonoTypeEnv,
    e: &NamedExpr,
    expected_ty: &Type<NamedVar>
) -> Result<Derivation<Judgment<NamedVar, Type<NamedVar>, Type<NamedVar>>>, String> {
    if let Expr::Fun(param, body) = e {
        if let Type::BinOp(ty1, _, ty2) = expected_ty {
            let mut new_env = env.clone();
            new_env.push((param.clone(), *ty1.clone()));
            let mut body_ctx = InferContext { sub: ctx.sub.clone(), var_counter: ctx.var_counter };
            let premise = check_expr(&mut body_ctx, &new_env, body, ty2.as_ref())?;
            ctx.sub = body_ctx.sub;
            return Ok(Derivation {
                judgment: Judgment { env: env.clone(), expr: e.clone(), result: expected_ty.clone() },
                rule: "T-Fun".to_string(),
                premises: vec![premise],
            });
        } else {
            return Err(format!("Type mismatch. Expression is a function, but expected type is {:?}.", expected_ty));
        }
    }

    let mut inferred_derivation = infer_expr(ctx, env, e)?;
    
    ctx.sub = unify(&inferred_derivation.judgment.result, expected_ty, &ctx.sub)?;

    inferred_derivation.judgment.result = apply_sub(expected_ty, &ctx.sub);
    Ok(inferred_derivation)
}

/// Helper function to apply substitutions to a type environment.
fn apply_sub_to_env(env: &MonoTypeEnv, sub: &Substitution) -> MonoTypeEnv {
    env.iter()
        .map(|(var, ty)| (var.clone(), apply_sub(ty, sub)))
        .collect()
}

/// Applies substitutions to all types within a derivation tree.
fn apply_sub_to_deriv(deriv: &mut Derivation<Judgment<NamedVar, Type<NamedVar>, Type<NamedVar>>>, sub: &Substitution) {
    let judgment = &mut deriv.judgment;
    judgment.result = apply_sub(&judgment.result, sub);
    judgment.env = apply_sub_to_env(&judgment.env, sub);

    for premise in &mut deriv.premises {
        apply_sub_to_deriv(premise, sub);
    }
}


/// The recursive helper that generates and solves type constraints.
fn infer_expr(
    ctx: &mut InferContext,
    env: &MonoTypeEnv,
    e: &NamedExpr
) -> Result<Derivation<Judgment<NamedVar, Type<NamedVar>, Type<NamedVar>>>, String> {
    match e {
        Expr::Int(_) => Ok(Derivation {
            judgment: Judgment { env: env.clone(), expr: e.clone(), result: Type::Int },
            rule: "T-Int".to_string(),
            premises: vec![],
        }),
        Expr::Bool(_) => Ok(Derivation {
            judgment: Judgment { env: env.clone(), expr: e.clone(), result: Type::Bool },
            rule: "T-Bool".to_string(),
            premises: vec![],
        }),
        Expr::Nil => {
            let elem_type = ctx.new_type_var();
            Ok(Derivation {
                judgment: Judgment { env: env.clone(), expr: e.clone(), result: Type::List(Box::new(elem_type)) },
                rule: "T-Nil".to_string(),
                premises: vec![],
            })
        }
        Expr::Var(var) => {
            for (v, ty) in env.iter().rev() {
                if v == var {
                    return Ok(Derivation {
                        judgment: Judgment { env: env.clone(), expr: e.clone(), result: ty.clone() },
                        rule: "T-Var".to_string(),
                        premises: vec![],
                    });
                }
            }
            Err(format!("Unbound variable: {}", var.0))
        }
        Expr::Group(inner_expr) => {
            let inner_deriv = infer_expr(ctx, env, inner_expr)?;
            Ok(Derivation {
                judgment: Judgment { env: env.clone(), expr: e.clone(), result: Type::Group(Box::new(inner_deriv.judgment.result.clone())) },
                rule: inner_deriv.rule.clone(),
                premises: inner_deriv.premises,
            })
        }
        Expr::Fun(param, body) => {
            let param_ty = ctx.new_type_var();
            let mut new_env = env.clone();
            new_env.push((param.clone(), param_ty.clone()));

            let body_deriv = infer_expr(ctx, &new_env, body)?;
            
            let fun_ty = Type::BinOp(Box::new(param_ty), Op::Fun, Box::new(body_deriv.judgment.result.clone()));
            
            Ok(Derivation {
                judgment: Judgment { env: env.clone(), expr: e.clone(), result: fun_ty },
                rule: "T-Fun".to_string(),
                premises: vec![body_deriv],
            })
        }
        Expr::App(e1, e2) => {
            let d1 = infer_expr(ctx, env, e1)?;
            let t1 = apply_sub(&d1.judgment.result, &ctx.sub);

            let d2 = infer_expr(ctx, env, e2)?;
            let t2 = apply_sub(&d2.judgment.result, &ctx.sub);

            let return_ty = ctx.new_type_var();
            let fun_ty = Type::BinOp(Box::new(t2), Op::Fun, Box::new(return_ty.clone()));
            
            ctx.sub = unify(&t1, &fun_ty, &ctx.sub)?;
            
            let final_type = apply_sub(&return_ty, &ctx.sub);
            Ok(Derivation {
                judgment: Judgment { env: env.clone(), expr: e.clone(), result: final_type },
                rule: "T-App".to_string(),
                premises: vec![d1, d2],
            })
        }
        Expr::Let(x, e1, e2) => {
            let d1 = infer_expr(ctx, env, e1)?;
            let t1 = apply_sub(&d1.judgment.result, &ctx.sub);

            let mut new_env = env.clone();
            new_env.push((x.clone(), t1));
            
            let d2 = infer_expr(ctx, &new_env, e2)?;
            
            Ok(Derivation {
                judgment: Judgment { env: env.clone(), expr: e.clone(), result: d2.judgment.result.clone() },
                rule: "T-Let".to_string(),
                premises: vec![d1, d2],
            })
        }
        Expr::BinOp(e1, op, e2) => {
            let d1 = infer_expr(ctx, env, e1)?;
            let t1 = apply_sub(&d1.judgment.result, &ctx.sub);

            let d2 = infer_expr(ctx, env, e2)?;
            let t2 = apply_sub(&d2.judgment.result, &ctx.sub);
            
            if let Op::App = op {
                let return_ty = ctx.new_type_var();
                let fun_ty = Type::BinOp(Box::new(t2), Op::Fun, Box::new(return_ty.clone()));
                
                ctx.sub = unify(&t1, &fun_ty, &ctx.sub)?;
                let final_type = apply_sub(&return_ty, &ctx.sub);
                
                return Ok(Derivation {
                    judgment: Judgment { env: env.clone(), expr: e.clone(), result: final_type },
                    rule: "T-App".to_string(),
                    premises: vec![d1, d2],
                });
            }

            let (expected_t1, expected_t2, result_ty, rule_name) = match op {
                Op::Add => (Type::Int, Type::Int, Type::Int, "T-Plus"),
                Op::Sub => (Type::Int, Type::Int, Type::Int, "T-Minus"),
                Op::Mul => (Type::Int, Type::Int, Type::Int, "T-Times"),
                Op::Lt => (Type::Int, Type::Int, Type::Bool, "T-Lt"),
                Op::Cons => {
                    let elem_type = ctx.new_type_var();
                    (elem_type.clone(), Type::List(Box::new(elem_type)), t2.clone(), "T-Cons")
                },
                _ => return Err(format!("Unhandled operator: {:?}", op)),
            };
            
            ctx.sub = unify(&t1, &expected_t1, &ctx.sub)?;
            ctx.sub = unify(&t2, &expected_t2, &ctx.sub)?;
            
            let final_type = apply_sub(&result_ty, &ctx.sub);
            Ok(Derivation {
                judgment: Judgment { env: env.clone(), expr: e.clone(), result: final_type },
                rule: rule_name.to_string(),
                premises: vec![d1, d2]
            })
        }
        Expr::If(cond, then_branch, else_branch) => {
            let d_cond = infer_expr(ctx, env, cond)?;
            ctx.sub = unify(&d_cond.judgment.result, &Type::Bool, &ctx.sub)?;
            
            let d_then = infer_expr(ctx, env, then_branch)?;
            let d_else = infer_expr(ctx, env, else_branch)?;

            ctx.sub = unify(&d_then.judgment.result, &d_else.judgment.result, &ctx.sub)?;
            let final_type = apply_sub(&d_then.judgment.result, &ctx.sub);

            Ok(Derivation {
                judgment: Judgment { env: env.clone(), expr: e.clone(), result: final_type },
                rule: "T-If".to_string(),
                premises: vec![d_cond, d_then, d_else],
            })
        }
        Expr::LetRec(f, x, e1, e2) => {
            let t1 = ctx.new_type_var();
            let t2 = ctx.new_type_var();
            let fun_ty = Type::BinOp(Box::new(t1.clone()), Op::Fun, Box::new(t2.clone()));
            
            let mut new_env1 = env.clone();
            new_env1.push((f.clone(), fun_ty.clone()));
            new_env1.push((x.clone(), t1));
            
            let d1 = infer_expr(ctx, &new_env1, e1)?;
            
            ctx.sub = unify(&d1.judgment.result, &t2, &ctx.sub)?;

            let mut new_env2 = env.clone();
            new_env2.push((f.clone(), fun_ty));

            let d2 = infer_expr(ctx, &new_env2, e2)?;
            
            Ok(Derivation {
                judgment: Judgment { env: env.clone(), expr: e.clone(), result: d2.judgment.result.clone() },
                rule: "T-LetRec".to_string(),
                premises: vec![d1, d2],
            })
        }
        Expr::Match(e1, e2, x, y, e3) => {
            let d1 = infer_expr(ctx, env, e1)?;
            let elem_ty = ctx.new_type_var();
            let list_ty = Type::List(Box::new(elem_ty.clone()));
            ctx.sub = unify(&d1.judgment.result, &list_ty, &ctx.sub)?;

            let d2 = infer_expr(ctx, env, e2)?;
            let t_nil = apply_sub(&d2.judgment.result, &ctx.sub);

            let mut new_env = env.clone();
            new_env.push((x.clone(), elem_ty.clone()));
            new_env.push((y.clone(), list_ty.clone()));

            let d3 = infer_expr(ctx, &new_env, e3)?;
            
            ctx.sub = unify(&t_nil, &d3.judgment.result, &ctx.sub)?;
            let final_type = apply_sub(&t_nil, &ctx.sub);

            Ok(Derivation {
                judgment: Judgment { env: env.clone(), expr: e.clone(), result: final_type },
                rule: "T-Match".to_string(),
                premises: vec![d1, d2, d3],
            })
        }
        _ => Err("This judgment type is not supported by the type checker.".to_string()),
    }
}

fn default_unconstrained_vars(t: &Type<NamedVar>) -> Type<NamedVar> {
    match t {
        Type::Var(_) => Type::Int, // Default hanging type variables to int
        Type::Fun(p, r) => Type::Fun(Box::new(default_unconstrained_vars(p)), Box::new(default_unconstrained_vars(r))),
        Type::List(inner) => Type::List(Box::new(default_unconstrained_vars(inner))),
        Type::BinOp(p, op , r) => Type::BinOp(
            Box::new(default_unconstrained_vars(p)), 
            op.clone(), 
            Box::new(default_unconstrained_vars(r))
        ),
        Type::Group(inner) => Type::Group(Box::new(default_unconstrained_vars(inner))),
        _ => t.clone(), // Concrete types (Int, Bool) remain unchanged.
    }
}

fn default_vars_in_deriv(deriv: &mut Derivation<Judgment<NamedVar, Type<NamedVar>, Type<NamedVar>>>) {
    let judgment = &mut deriv.judgment;
    judgment.result = default_unconstrained_vars(&judgment.result);
    judgment.env = judgment.env.iter().map(|(v, t)| (v.clone(), default_unconstrained_vars(t))).collect();
    for premise in &mut deriv.premises {
        default_vars_in_deriv(premise);
    }
}