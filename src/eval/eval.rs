// src/ml4.rs

use crate::eval::ast::{Expr, Value, Env, Op, LanguageVersion, Var};
use crate::eval::proof::{Derivation, Axiom};

use std::fmt;
use std::rc::Rc;


impl Derivation {
        fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent_level: usize) -> fmt::Result {
        let indent_str = "    ".repeat(indent_level);

        if let Some(ax_str) = self.to_axiom_string() {
            writeln!(f, "{}{}", indent_str, ax_str)?;
            return Ok(());
        }

        writeln!(
            f,
            "{}{}{} evalto {} by {} {{",
            indent_str,
            format_env(&self.env, self.version),
            self.expr.to_ml4_string(),
            self.result.to_ml4_string(),
            self.rule
        )?;

        for sub in &self.sub_derivations {
            sub.fmt_with_indent(f, indent_level + 1)?;
        }

        writeln!(f, "{}}};", indent_str)
    }
}

impl fmt::Display for Derivation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}

impl Axiom for Derivation {
    fn to_axiom_string(&self) -> Option<String> {
        if !self.rule.starts_with("B-") {
            return None;
        }

        if let Expr::BinOp(lhs_expr, op, rhs_expr, _) = &self.expr {
            let lhs_val = match &**lhs_expr {
                Expr::Int(n) => Value::Int(*n),
                _ => return None,
            };
            let rhs_val = match &**rhs_expr {
                Expr::Int(n) => Value::Int(*n),
                _ => return None,
            };

            let op_word = match op {
                Op::Add => "plus",
                Op::Sub => "minus",
                Op::Mul => "times",
                Op::Lt => "less than",
                Op::Cons => "cons",
            };

            return Some(format!(
                " {} {} {} is {} by {} {{}};",
                lhs_val.to_ml4_string(),
                op_word,
                rhs_val.to_ml4_string(),
                self.result.to_ml4_string(),
                self.rule
            ));
        }

        None
    }
}

fn format_env(env: &Env, version: LanguageVersion) -> String {
    if version == LanguageVersion::ML1 {
        return String::new();
    }

    if env.is_empty() {
        String::from("|- ")
    } else {
        let binds = env
            .iter()
            .map(|(v, val)| format!("{} = {}", v.0, val.to_ml4_string()))
            .collect::<Vec<_>>()
            .join(", ");
        format!("{} |- ", binds)
    }
}

pub trait ToML4String {
    fn to_ml4_string(&self) -> String;
}

impl ToML4String for Expr {
    fn to_ml4_string(&self) -> String {
        match self {
            Expr::Int(i) => i.to_string(),
            Expr::Bool(b) => b.to_string(),
            Expr::Var(v) => v.0.clone(),
            Expr::Let(v, e1, e2, is_paren) => {
                let s = format!("let {} = {} in {}", v.0, e1.to_ml4_string(), e2.to_ml4_string());
                if *is_paren { format!("({})", s) } else { s }
            }
            Expr::LetRec(f, x, body, e2, is_paren) => {
                let s = format!("let rec {} = fun {} -> {} in {}", f.0, x.0, body.to_ml4_string(), e2.to_ml4_string());
                if *is_paren { format!("({})", s) } else { s }
            },
            Expr::If(c, t, e, is_paren) => {
                let s = format!("if {} then {} else {}", c.to_ml4_string(), t.to_ml4_string(), e.to_ml4_string());
                if *is_paren { format!("({})", s) } else { s }
            },
            Expr::BinOp(e1, op, e2, is_paren) => {
                let s = format!("{} {} {}", e1.to_ml4_string(), op.to_ml4_string(), e2.to_ml4_string());
                if *is_paren { format!("({})", s) } else { s }
            }
            Expr::App(f, arg, is_paren) => {
                let s = format!("{} {}", f.to_ml4_string(), arg.to_ml4_string());
                if *is_paren { format!("({})", s) } else { s }
            }
            Expr::Fun(param, body, is_paren) => {
                let s = format!("fun {} -> {}", param.0, body.to_ml4_string());
                if *is_paren { format!("({})", s) } else { s }
            },
            Expr::Nil => "[]".to_string(),
            Expr::Cons(h, t, is_paren) => {
                let s = format!("{} :: {}", h.to_ml4_string(), t.to_ml4_string());
                if *is_paren { format!("({})", s) } else { s }
            },
            Expr::Match(e, nil_case, hd, tl, cons_case, is_paren) => {
                let s = format!(
                    "match {} with [] -> {} | {} :: {} -> {}",
                    e.to_ml4_string(),
                    nil_case.to_ml4_string(),
                    hd.0,
                    tl.0,
                    cons_case.to_ml4_string()
                );
                if *is_paren { format!("({})", s) } else { s }
            },
        }
    }
}

impl ToML4String for Value {
    fn to_ml4_string(&self) -> String {
        match self {
            Value::Int(i) => {
                let s = i.to_string();
                s
            }
            Value::Bool(b) => {
                let s = b.to_string();
                s
            }
            Value::Nil => {
                let s = "[]".to_string();
                s
            }
            Value::Cons(h, t, is_paren) => {
                let s = format!("{} :: {}", h.to_ml4_string(), t.to_ml4_string());
                if *is_paren { format!("({})", s) } else { s }
            }
            Value::FunVal(param, body, env, _) => {
                let env_str = if env.is_empty() {
                    "()".to_string()
                } else {
                    format!("({})", env.iter()
                        .map(|(v, val)| format!("{} = {}", v.0, val.to_ml4_string()))
                        .collect::<Vec<_>>().join(", "))
                };
                let s = format!("{}[fun {} -> {}]", env_str, param.0, body.to_ml4_string());
                s
            }
            Value::RecFunVal(f, x, body, env, _) => {
                let env_str = if env.is_empty() {
                    "()".to_string()
                } else {
                    format!("({})", env.iter()
                        .map(|(v, val)| format!("{} = {}", v.0, val.to_ml4_string()))
                        .collect::<Vec<_>>().join(", "))
                };
                let s = format!("{}[rec {} = fun {} -> {}]", env_str, f.0, x.0, body.to_ml4_string());
                s
            }
        }
    }
}

impl ToML4String for Op {
    fn to_ml4_string(&self) -> String {
        match self {
            Op::Add => "+",
            Op::Sub => "-",
            Op::Mul => "*",
            Op::Cons => "::",
            Op::Lt => "<",
        }
        .to_string()
    }
}

pub fn derive(env: &Env, expr: &Expr, version: LanguageVersion) -> Derivation {
    match expr {
        Expr::Int(i) => Derivation {
            env: Rc::clone(env),
            expr: expr.clone(),
            result: Value::Int(*i),
            rule: "E-Int".to_string(),
            sub_derivations: vec![],
            version,
        },
        Expr::Bool(b) => Derivation {
            env: Rc::clone(env),
            expr: expr.clone(),
            result: Value::Bool(*b),
            rule: "E-Bool".to_string(),
            sub_derivations: vec![],
            version,
        },
        Expr::Var(x) => match version {
            LanguageVersion::ML1 => {
                panic!("Error: Variables are not supported in ML1 (found: {})", x.0);
            }
            LanguageVersion::ML2 | LanguageVersion::ML3 => {
                fn derive_var_recursive(env: &Env, current_expr: &Expr, x: &Var, version: LanguageVersion) -> Derivation {
                    if env.is_empty() {
                        panic!("Unbound variable: {}", x.0);
                    }
                    let last_index = env.len() - 1;
                    let (last_var, last_val) = &env[last_index];
                    if last_var == x {
                        Derivation {
                            env: Rc::clone(env),
                            expr: current_expr.clone(),
                            result: last_val.clone(),
                            rule: "E-Var1".to_string(),
                            sub_derivations: vec![],
                            version,
                        }
                    } else {
                        let sub_env = Rc::new(env[..last_index].to_vec());
                        let sub_derivation = derive_var_recursive(&sub_env, current_expr, x, version);
                        Derivation {
                            env: Rc::clone(env),
                            expr: current_expr.clone(),
                            result: sub_derivation.result.clone(),
                            rule: "E-Var2".to_string(),
                            sub_derivations: vec![sub_derivation],
                            version,
                        }
                    }
                }
                derive_var_recursive(env, expr, x, version)
            }
            LanguageVersion::ML4 => {
                for (v, val) in env.iter().rev() {
                    if v == x {
                        return Derivation {
                            env: Rc::clone(env),
                            expr: expr.clone(),
                            result: val.clone(),
                            rule: "E-Var".to_string(),
                            sub_derivations: vec![],
                            version,
                        };
                    }
                }
                panic!("Unbound variable: {}", x.0)
            }
        },
        Expr::BinOp(e1, op, e2, is_paren) => {
            let d1 = derive(env, e1, version);
            let d2 = derive(env, e2, version);
            let (v1, v2) = (d1.result.clone(), d2.result.clone());

            let (result, rule, basic_rule) = match (v1.clone(), v2.clone(), op) {
                (Value::Int(i1), Value::Int(i2), Op::Add) => (
                    Value::Int(i1 + i2),
                    "E-Plus",
                    Some(Derivation {
                        env: Rc::new(vec![]),
                        expr: Expr::BinOp(Box::new(Expr::Int(i1)), Op::Add, Box::new(Expr::Int(i2)), *is_paren),
                        result: Value::Int(i1 + i2),
                        rule: "B-Plus".to_string(),
                        sub_derivations: vec![],
                        version,
                    }),
                ),
                (Value::Int(i1), Value::Int(i2), Op::Sub) => (
                    Value::Int(i1 - i2),
                    "E-Minus",
                    Some(Derivation {
                        env: Rc::new(vec![]),
                        expr: Expr::BinOp(Box::new(Expr::Int(i1)), Op::Sub, Box::new(Expr::Int(i2)), *is_paren),
                        result: Value::Int(i1 - i2),
                        rule: "B-Minus".to_string(),
                        sub_derivations: vec![],
                        version,
                    }),
                ),
                (Value::Int(i1), Value::Int(i2), Op::Mul) => (
                    Value::Int(i1 * i2),
                    "E-Times",
                    Some(Derivation {
                        env: Rc::new(vec![]),
                        expr: Expr::BinOp(Box::new(Expr::Int(i1)), Op::Mul, Box::new(Expr::Int(i2)), *is_paren),
                        result: Value::Int(i1 * i2),
                        rule: "B-Times".to_string(),
                        sub_derivations: vec![],
                        version,
                    }),
                ),
                (Value::Int(i1), Value::Int(i2), Op::Lt) => (
                    Value::Bool(i1 < i2),
                    "E-Lt",
                    Some(Derivation {
                        env: Rc::new(vec![]),
                        expr: Expr::BinOp(Box::new(Expr::Int(i1)), Op::Lt, Box::new(Expr::Int(i2)), *is_paren),
                        result: Value::Bool(i1 < i2),
                        rule: "B-Lt".to_string(),
                        sub_derivations: vec![],
                        version,
                    }),
                ),
                (v_head, v_tail, Op::Cons) => (
                    Value::Cons(Box::new(v_head.clone()), Box::new(v_tail.clone()), *is_paren),
                    "E-Cons",
                    None,
                ),
                _ => panic!("Invalid binary op eval {:?}", op),
            };

            let mut sub_derivations = vec![d1, d2];
            if let Some(basic) = basic_rule {
                sub_derivations.push(basic);
            }

            Derivation {
                env: Rc::clone(env),
                expr: expr.clone(),
                result,
                rule: rule.to_string(),
                sub_derivations,
                version,
            }
        }
        Expr::If(cond, e_then, e_else, _) => {
            let d_cond = derive(env, cond, version);
            match d_cond.result {
                Value::Bool(true) => {
                    let d_then = derive(env, e_then, version);
                    Derivation {
                        env: Rc::clone(env),
                        expr: expr.clone(),
                        result: d_then.result.clone(),
                        rule: "E-IfT".to_string(),
                        sub_derivations: vec![d_cond, d_then],
                        version,
                    }
                }
                Value::Bool(false) => {
                    let d_else = derive(env, e_else, version);
                    Derivation {
                        env: Rc::clone(env),
                        expr: expr.clone(),
                        result: d_else.result.clone(),
                        rule: "E-IfF".to_string(),
                        sub_derivations: vec![d_cond, d_else],
                        version,
                    }
                }
                _ => panic!("Condition must evaluate to a boolean"),
            }
        }
        Expr::Let(x, e1, e2, _) => {
            let d1 = derive(env, e1, version);
            let mut new_env = (**env).clone();
            new_env.push((x.clone(), d1.result.clone()));
            let rc_env = Rc::new(new_env);
            let d2 = derive(&rc_env, e2, version);
            Derivation {
                env: Rc::clone(env),
                expr: expr.clone(),
                result: d2.result.clone(),
                rule: "E-Let".to_string(),
                sub_derivations: vec![d1, d2],
                version,
            }
        }
        Expr::Fun(param, body, is_paren) => {
            Derivation {
                env: Rc::clone(env),
                expr: expr.clone(),
                result: Value::FunVal(param.clone(), body.clone(), Rc::clone(env), *is_paren),
                rule: "E-Fun".to_string(),
                sub_derivations: vec![],
                version,
            }
        }
        Expr::App(f, arg, is_paren) => {
            let df = derive(env, f, version);
            let darg = derive(env, arg, version);
            let result;
            let sub_derivations;
            match &df.result {
                Value::FunVal(param, body, closure_env, _) => {
                    let mut new_env = (**closure_env).to_vec();
                    new_env.push((param.clone(), darg.result.clone()));
                    let rc_env = Rc::new(new_env);
                    let d_body = derive(&rc_env, &body, version);
                    result = d_body.result.clone();
                    sub_derivations = vec![df, darg, d_body];
                    Derivation {
                        env: Rc::clone(env),
                        expr: Expr::App(f.clone(), arg.clone(), *is_paren),
                        result,
                        rule: "E-App".to_string(),
                        sub_derivations,
                        version,
                    }
                }
                Value::RecFunVal(name, param, body, closure_env,is_paren_2) => {
                    let mut new_env = (**closure_env).to_vec();
                    new_env.push((name.clone(), Value::RecFunVal(name.clone(), param.clone(), body.clone(), closure_env.clone(), *is_paren_2)));
                    new_env.push((param.clone(), darg.result.clone()));
                    let rc_env = Rc::new(new_env);
                    let d_body = derive(&rc_env, &body, version);
                    result = d_body.result.clone();
                    sub_derivations = vec![df, darg, d_body];
                    Derivation {
                        env: Rc::clone(env),
                        expr: Expr::App(f.clone(), arg.clone(), *is_paren),
                        result,
                        rule: "E-AppRec".to_string(),
                        sub_derivations,
                        version,
                    }
                }
                _ => panic!("Tried to apply non-function"),
            }
        }
        Expr::LetRec(f, x, body, e2,is_paren) => {
            let mut new_env = (**env).clone();
            let rec_val = Value::RecFunVal(f.clone(), x.clone(), body.clone(), Rc::new(new_env.clone()), *is_paren);
            new_env.push((f.clone(), rec_val.clone()));
            let rc_env = Rc::new(new_env);
            let d2 = derive(&rc_env, e2, version);
            Derivation {
                env: Rc::clone(env),
                expr: expr.clone(),
                result: d2.result.clone(),
                rule: "E-LetRec".to_string(),
                sub_derivations: vec![d2],
                version,
            }
        }
        Expr::Nil => Derivation {
            env: Rc::clone(env),
            expr: expr.clone(),
            result: Value::Nil,
            rule: "E-Nil".to_string(),
            sub_derivations: vec![],
            version,
        },
        Expr::Cons(e1, e2, is_paren) => {
            let d1 = derive(env, e1, version);
            let d2 = derive(env, e2, version);
            Derivation {
                env: Rc::clone(env),
                expr: expr.clone(),
                result: Value::Cons(Box::new(d1.result.clone()), Box::new(d2.result.clone()), *is_paren),
                rule: "E-Cons".to_string(),
                sub_derivations: vec![d1, d2],
                version,
            }
        },

        Expr::Match(e, e_nil, x, y, e_cons, _) => {
            let d_expr = derive(env, e, version);
            match d_expr.result.clone() {
                Value::Nil => {
                    let d_nil = derive(env, e_nil, version);
                    Derivation {
                        env: Rc::clone(env),
                        expr: expr.clone(),
                        result: d_nil.result.clone(),
                        rule: "E-MatchNil".to_string(),
                        sub_derivations: vec![d_expr, d_nil],
                        version,
                    }
                }
                Value::Cons(v1, v2, _) => {
                    let mut new_env = (**env).clone();
                    new_env.push((x.clone(), *v1));
                    new_env.push((y.clone(), *v2));
                    let rc_env = Rc::new(new_env);
                    let d_cons = derive(&rc_env, e_cons, version);
                    Derivation {
                        env: Rc::clone(env),
                        expr: expr.clone(),
                        result: d_cons.result.clone(),
                        rule: "E-MatchCons".to_string(),
                        sub_derivations: vec![d_expr, d_cons],
                        version,
                    }
                }
                _ => panic!("Cannot match on non-list value"),
            }
        },
    }
} 
