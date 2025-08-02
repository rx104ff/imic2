// src/nameless/eval.rs
use crate::{
    common::{
        ast::{
            core::{DBIndex, NamelessVar, Op}, env::{Env, NamelessEnv}, expr::{Expr, NamelessExpr}, judgement::Judgment as GlobalJudgment, value::NamelessValue
        },
        proof::{Derivation, Judgment},
    },
    nameless::proof::Axiom,
};

type NamelessDerivation = Derivation<Judgment<NamelessVar, NamelessValue, NamelessValue>>;

pub fn derive_judgement(judgment: &GlobalJudgment) -> Result<NamelessDerivation, String> {
    match judgment {
        GlobalJudgment::NamelessEvaluation(env, expr) => derive(env, expr),
        _ => Err("This judgment type is not supported by the type checker.".to_string()),
    }
}

pub fn derive(env: &NamelessEnv<NamelessValue>, expr: &NamelessExpr) -> Result<NamelessDerivation, String> {
    match expr {
        Expr::Int(i) => Ok(Derivation {
            judgment: Judgment { env: env.clone(), expr: expr.clone(), result: NamelessValue::Int(*i) },
            rule: "E-Int".to_string(),
            premises: vec![],
        }),
        Expr::Bool(b) => Ok(Derivation {
            judgment: Judgment { env: env.clone(), expr: expr.clone(), result: NamelessValue::Bool(*b) },
            rule: "E-Bool".to_string(),
            premises: vec![],
        }),
        Expr::Var(NamelessVar(nameless_var)) => {
            let i = nameless_var.0;
            if i > 0 && i <= env.len() {
                let val = env[env.len() - i].1.clone();
                Ok(Derivation {
                    judgment: Judgment { env: env.clone(), expr: expr.clone(), result: val },
                    rule: "E-Var".to_string(),
                    premises: vec![],
                })
            } else {
                Err(format!("Unbound variable index: #{}", i))
            }
        }
        Expr::Group(e) => derive(env, e),
        Expr::Let(_, e1, e2) => {
            let d1 = derive(env, e1)?;
            let mut new_env = env.clone();
            new_env.push((NamelessVar(DBIndex(env.len() + 1)), d1.judgment.result.clone()));
            let d2 = derive(&new_env, e2)?;
            Ok(Derivation {
                judgment: Judgment { env: env.clone(), expr: expr.clone(), result: d2.judgment.result.clone() },
                rule: "E-Let".to_string(),
                premises: vec![d1, d2],
            })
        },
        Expr::Fun(var, body) => {
            let result = NamelessValue::FunVal(var.clone(), body.clone(), env.clone());
            Ok(Derivation {
                judgment: Judgment { env: env.clone(), expr: expr.clone(), result },
                rule: "E-Fun".to_string(),
                premises: vec![],
            })
        },
        Expr::If(cond, e_then, e_else) => {
            let d_cond = derive(env, cond)?;
            match d_cond.judgment.result {
                NamelessValue::Bool(true) => {
                    let d_then = derive(env, e_then)?;
                    Ok(Derivation {
                        judgment: Judgment { env: env.clone(), expr: expr.clone(), result: d_then.judgment.result.clone() },
                        rule: "E-IfT".to_string(),
                        premises: vec![d_cond, d_then],
                    })
                }
                NamelessValue::Bool(false) => {
                    let d_else = derive(env, e_else)?;
                    Ok(Derivation {
                        judgment: Judgment { env: env.clone(), expr: expr.clone(), result: d_else.judgment.result.clone() },
                        rule: "E-IfF".to_string(),
                        premises: vec![d_cond, d_else],
                    })
                }
                _ => Err("Condition for an 'if' expression must evaluate to a boolean.".to_string()),
            }
        },
        Expr::LetRec(f, x, body, e2) => {
            let mut new_env = env.clone();
            let rec_val = NamelessValue::RecFunVal(f.clone(), x.clone(), body.clone(), env.clone());
            new_env.push((f.clone(), rec_val.clone()));
            let d2 = derive(&new_env, e2)?;
            Ok(Derivation {
                judgment: Judgment { env: env.clone(), expr: expr.clone(), result: d2.judgment.result.clone() },
                rule: "E-LetRec".to_string(),
                premises: vec![d2],
            })
        },
        Expr::App(f_expr, arg_expr) => {
            let d_f = derive(env, f_expr)?;
            let d_arg = derive(env, arg_expr)?;

            match &d_f.judgment.result {
                NamelessValue::FunVal(binder, body, closure_env) => {
                    let mut new_env = closure_env.clone();
                    new_env.push((binder.clone(), d_arg.judgment.result.clone()));
                    let d_body = derive(&new_env, body)?;
                    Ok(Derivation {
                        judgment: Judgment { env: env.clone(), expr: expr.clone(), result: d_body.judgment.result.clone() },
                        rule: "E-App".to_string(),
                        premises: vec![d_f, d_arg, d_body],
                    })
                }
                NamelessValue::RecFunVal(f, x, body, closure_env) => {
                    let mut new_env = closure_env.clone();
                    let rec_val = NamelessValue::RecFunVal(f.clone(), x.clone(), body.clone(), closure_env.clone());
                    new_env.push((f.clone(), rec_val));
                    new_env.push((x.clone(), d_arg.judgment.result.clone()));
                    let d_body = derive(&new_env, body)?;
                    Ok(Derivation {
                        judgment: Judgment { env: env.clone(), expr: expr.clone(), result: d_body.judgment.result.clone() },
                        rule: "E-AppRec".to_string(),
                        premises: vec![d_f, d_arg, d_body],
                    })
                }
                _ => Err("Attempted to apply a non-function value".to_string()),
            }
        }
        Expr::BinOp(e1, op, e2) => {
            let d1 = derive(env, e1)?;
            let d2 = derive(env, e2)?;

            let v1 = d1.judgment.result.clone();
            let v2 = d2.judgment.result.clone();

            let (result, rule, basic_rule) = match (&v1, &v2, op) {
                 (NamelessValue::Int(i1), NamelessValue::Int(i2), Op::Add) => {
                    let res = NamelessValue::Int(i1 + i2);
                    let axiom = Derivation {
                        judgment: Judgment { env: Env(vec![]), expr: Expr::BinOp(Box::new(Expr::Int(*i1)), Op::Add, Box::new(Expr::Int(*i2))), result: res.clone() },
                        rule: "B-Plus".to_string(), premises: vec![]
                    };
                    (res, "E-Plus", Some(axiom))
                }
                (NamelessValue::Int(i1), NamelessValue::Int(i2), Op::Sub) => {
                    let res = NamelessValue::Int(i1 - i2);
                    let axiom = Derivation {
                        judgment: Judgment { env: Env(vec![]), expr: Expr::BinOp(Box::new(Expr::Int(*i1)), Op::Sub, Box::new(Expr::Int(*i2))), result: res.clone() },
                        rule: "B-Minus".to_string(), premises: vec![]
                    };
                    (res, "E-Minus", Some(axiom))
                }
                (NamelessValue::Int(i1), NamelessValue::Int(i2), Op::Mul) => {
                    let res = NamelessValue::Int(i1 * i2);
                     let axiom = Derivation {
                        judgment: Judgment { env: Env(vec![]), expr: Expr::BinOp(Box::new(Expr::Int(*i1)), Op::Mul, Box::new(Expr::Int(*i2))), result: res.clone() },
                        rule: "B-Times".to_string(), premises: vec![]
                    };
                    (res, "E-Times", Some(axiom))
                }
                (NamelessValue::Int(i1), NamelessValue::Int(i2), Op::Lt) => {
                    let res = NamelessValue::Bool(i1 < i2);
                     let axiom = Derivation {
                        judgment: Judgment { env: Env(vec![]), expr: Expr::BinOp(Box::new(Expr::Int(*i1)), Op::Lt, Box::new(Expr::Int(*i2))), result: res.clone() },
                        rule: "B-Lt".to_string(), premises: vec![]
                    };
                    (res, "E-Lt", Some(axiom))
                }
                _ => return Err(format!("Invalid operands for binary operator {:?}", op)),
            };

            let mut premises = vec![d1, d2];
            if let Some(basic) = basic_rule {
                premises.push(basic);
            }

            Ok(Derivation {
                judgment: Judgment { env: env.clone(), expr: expr.clone(), result },
                rule: rule.to_string(),
                premises,
            })
        }
        _ => Err(format!("Nameless evaluation not implemented for: {}", expr)),
    }
}