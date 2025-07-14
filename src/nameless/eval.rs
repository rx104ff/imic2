// src/nameless_eval/eval.rs
use crate::common::ast::{DBIndex, Expr, Judgment, NamelessEnv, NamelessExpr, NamelessValue, NamelessVar, Op};
use crate::nameless::proof::Derivation;


pub fn derive_judgement(judgment: &Judgment) -> Result<Derivation, String> {
    match judgment {
        Judgment::NamelessEvaluation(env, expr) => {
            derive(env, expr)
        },
        _ => Err("This judgment type is not supported by the type checker.".to_string()),
    }
}

pub fn derive(env: &NamelessEnv, expr: &NamelessExpr) -> Result<Derivation, String> {
    match expr {
        Expr::Int(i) => Ok(Derivation {
            env: env.clone(),
            expr: expr.clone(),
            result: NamelessValue::Int(*i),
            rule: "E-Int".to_string(),
            sub_derivations: vec![],
        }),
        Expr::Bool(b) => Ok(Derivation {
            env: env.clone(),
            expr: expr.clone(),
            result: NamelessValue::Bool(*b),
            rule: "E-Bool".to_string(),
            sub_derivations: vec![],
        }),
        Expr::BinOp(e1, op, e2, is_paren) => {
            let d1 = derive(env, e1)?;
            let d2 = derive(env, e2)?;
            let (v1, v2) = (d1.result.clone(), d2.result.clone());

            let (result, rule, basic_rule) = match (&v1, &v2, op) {
                (NamelessValue::Int(i1), NamelessValue::Int(i2), Op::Add) => {
                    let res = NamelessValue::Int(i1 + i2);
                    let axiom = Derivation {
                        env: env.clone(),
                        expr: Expr::BinOp(Box::new(Expr::Int(*i1)), Op::Add, Box::new(Expr::Int(*i2)), *is_paren),
                        result: res.clone(),
                        rule: "B-Plus".to_string(),
                        sub_derivations: vec![],
                    };
                    (res, "E-Plus", Some(axiom))
                }
                (NamelessValue::Int(i1), NamelessValue::Int(i2), Op::Sub) => {
                    let res = NamelessValue::Int(i1 - i2);
                    let axiom = Derivation {
                        env: env.clone(),
                        expr: Expr::BinOp(Box::new(Expr::Int(*i1)), Op::Sub, Box::new(Expr::Int(*i2)), *is_paren),
                        result: res.clone(),
                        rule: "B-Minus".to_string(),
                        sub_derivations: vec![],
                    };
                    (res, "E-Minus", Some(axiom))
                }
                (NamelessValue::Int(i1), NamelessValue::Int(i2), Op::Mul) => {
                    let res = NamelessValue::Int(i1 * i2);
                     let axiom = Derivation {
                        env: env.clone(),
                        expr: Expr::BinOp(Box::new(Expr::Int(*i1)), Op::Mul, Box::new(Expr::Int(*i2)), *is_paren),
                        result: res.clone(),
                        rule: "B-Times".to_string(),
                        sub_derivations: vec![],
                    };
                    (res, "E-Times", Some(axiom))
                }
                (NamelessValue::Int(i1), NamelessValue::Int(i2), Op::Lt) => {
                    let res = NamelessValue::Bool(i1 < i2);
                     let axiom = Derivation {
                        env: env.clone(),
                        expr: Expr::BinOp(Box::new(Expr::Int(*i1)), Op::Lt, Box::new(Expr::Int(*i2)), *is_paren),
                        result: res.clone(),
                        rule: "B-Lt".to_string(),
                        sub_derivations: vec![],
                    };
                    (res, "E-Lt", Some(axiom))
                }
                _ => return Err(format!("Invalid operands for binary operator {:?}", op)),
            };

            let mut sub_derivations = vec![d1, d2];
            if let Some(basic) = basic_rule {
                sub_derivations.push(basic);
            }

            Ok(Derivation {
                env: env.clone(),
                expr: expr.clone(),
                result,
                rule: rule.to_string(),
                sub_derivations,
            })
        }
        Expr::Var(NamelessVar(nameless_var)) => {
            let i = (nameless_var.0);
            // de Bruijn indices are 1-based from the user's perspective.
            if i > 0 && i <= env.len() {
                // #1 refers to the last item in the env, so we access `env[len - i]`.
                let val = env[env.len() - i].clone().1;
                Ok(Derivation {
                    env: env.clone(),
                    expr: expr.clone(),
                    result: val,
                    rule: "E-Var".to_string(),
                    sub_derivations: vec![],
                })
            } else {
                Err(format!("Unbound variable index: #{}", i))
            }
        }
        Expr::Let(_, e1, e2, _) => {
            let d1 = derive(env, e1)?;
            let mut new_env = env.clone();
            new_env.push((NamelessVar(DBIndex(env.len() + 1)), d1.result.clone())); // Add the new value to the environment
            let d2 = derive(&new_env, e2)?;
            Ok(Derivation {
                env: env.clone(),
                expr: expr.clone(),
                result: d2.result.clone(),
                rule: "E-Let".to_string(),
                sub_derivations: vec![d1, d2],
            })
        }
        Expr::Fun(var, body, is_paren) => {
            Ok(Derivation {
                env: env.clone(),
                expr: expr.clone(),
                result: NamelessValue::FunVal(var.clone(), body.clone(), env.clone(), *is_paren),
                rule: "E-Fun".to_string(),
                sub_derivations: vec![],
            })
        }
        Expr::If(cond, e_then, e_else, _) => {
            let d_cond = derive(env, cond)?;
            match d_cond.result {
                NamelessValue::Bool(true) => {
                    let d_then = derive(env, e_then)?;
                    Ok(Derivation {
                        env: env.clone(),
                        expr: expr.clone(),
                        result: d_then.result.clone(),
                        rule: "E-IfT".to_string(),
                        sub_derivations: vec![d_cond, d_then],
                    })
                }
                NamelessValue::Bool(false) => {
                    let d_else = derive(env, e_else)?;
                    Ok(Derivation {
                        env: env.clone(),
                        expr: expr.clone(),
                        result: d_else.result.clone(),
                        rule: "E-IfF".to_string(),
                        sub_derivations: vec![d_cond, d_else],
                    })
                }
                _ => Err("Condition for an 'if' expression must evaluate to a boolean.".to_string()),
            }
        }
        Expr::LetRec(f, x, body, e2, is_paren) => {
            let mut new_env = env.clone();
            let rec_val = NamelessValue::RecFunVal(f.clone(), x.clone(), body.clone(), env.clone(), *is_paren);
            // Push the recursive value onto the stack for the `in` part.
            new_env.push((f.clone(), rec_val.clone()));

            let d2 = derive(&new_env, e2)?;
            Ok(Derivation {
                env: env.clone(),
                expr: expr.clone(),
                result: d2.result.clone(),
                rule: "E-LetRec".to_string(),
                sub_derivations: vec![d2],
            })
        }
        Expr::App(f_expr, arg_expr, _) => {
            let d_f = derive(env, f_expr)?;
            let d_arg = derive(env, arg_expr)?;
            let result;
            let rule;
            let sub_derivations;

            match &d_f.result {
                NamelessValue::FunVal(binder, body, closure_env, _) => {
                    rule = "E-App".to_string();
                    let mut new_env = closure_env.clone();
                    // Push argument for the body.
                    new_env.push((binder.clone(), d_arg.result.clone()));

                    let d_body = derive(&new_env, body)?;
                    result = d_body.result.clone();
                    sub_derivations = vec![d_f, d_arg, d_body];
                }
                NamelessValue::RecFunVal(f, x, body, closure_env, is_paren_rec) => {
                    // Use the distinct E-AppRec rule.
                    rule = "E-AppRec".to_string();
                    let mut new_env = closure_env.clone();

                    // Create the recursive value to push into the environment.
                    let rec_val = NamelessValue::RecFunVal(f.clone(), x.clone(), body.clone(), closure_env.clone(), *is_paren_rec);

                    // Push the function itself first for recursion, then the argument.
                    // In the body: #1 will be the argument, #2 will be the recursive function.
                    new_env.push((f.clone(), rec_val));
                    new_env.push((x.clone(), d_arg.result.clone()));

                    let d_body = derive(&new_env, body)?;
                    result = d_body.result.clone();
                    sub_derivations = vec![d_f, d_arg, d_body];
                }
                _ => return Err("Attempted to apply a non-function value".to_string()),
            }
            Ok(Derivation {
                env: env.clone(),
                expr: expr.clone(),
                result,
                rule, // The rule is now correctly set to E-App or E-AppRec.
                sub_derivations,
            })
        }
        // ... Implement other expression types like BinOp, If, etc.
        _ => Err(format!("Nameless evaluation not implemented for: {}", expr)),
    }
}