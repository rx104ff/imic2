use std::fmt;
use crate::nat::ast::{ArithmeticOp, Derivation, Judgment, ComparisonMode, Nat, Expr};

impl fmt::Display for Derivation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn fmt_with_indent(d: &Derivation, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
            let prefix = "  ".repeat(indent);
            writeln!(f, "{}{} by {} {{", prefix, d.conclusion, d.rule)?;
            for premise in &d.premises {
                fmt_with_indent(premise, f, indent + 1)?;
            }
            writeln!(f, "{}}};", prefix)
        }
        fmt_with_indent(self, f, 0)
    }
}

impl fmt::Display for Nat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Nat::Z => write!(f, "Z"),
            Nat::S(n) => write!(f, "S({})", n),
        }
    }
}

impl fmt::Display for Judgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Judgment::Arithmetic { op, n1, n2, n3 } => {
                let op_str = match op {
                    ArithmeticOp::Plus => "plus",
                    ArithmeticOp::Times => "times",
                };
                write!(f, "{} {} {} is {}", n1, op_str, n2, n3)
            }
            Judgment::Comparison { n1, n2 } => {
                write!(f, "{} is less than {}", n1, n2)
            }
            Judgment::Evaluation { exp , n } => write!(f, "{} evalto {}", exp, n),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::N(n) => write!(f, "{}", n),
            Expr::Plus(e1, e2) => write!(f, "({} + {})", e1, e2),
            Expr::Times(e1, e2) => write!(f, "({} * {})", e1, e2),
        }
    }
}


pub fn derive(j: &Judgment, mode: Option<ComparisonMode>) -> Result<Derivation, String> {
    match j {
        Judgment::Arithmetic { op, .. } => match op {
            ArithmeticOp::Plus => derive_plus(j),
            ArithmeticOp::Times => derive_times(j),
        },
        Judgment::Comparison { .. } => {
            match mode {
                Some(ComparisonMode::V1) => derive_compare_v1(j),
                Some(ComparisonMode::V2) => derive_compare_v2(j),
                Some(ComparisonMode::V3) => derive_compare_v3(j),
                None => Err("Cannot prove comparison judgment: No comparison mode was specified. Try adding CompareNat1, CompareNat2, or CompareNat3 as the first argument.".to_string()),
            }
        },
        Judgment::Evaluation { exp, n } => derive_eval(exp, n),
    }
}

fn eval_expr(exp: &Expr) -> Nat {
    match exp {
        Expr::N(n) => n.clone(),
        Expr::Plus(e1, e2) => eval_expr(e1).plus(&eval_expr(e2)),
        Expr::Times(e1, e2) => eval_expr(e1).times(&eval_expr(e2)),
    }
}

/// Attempts to prove a "plus" judgment.
fn derive_plus(j: &Judgment) -> Result<Derivation, String> {
    let (n1, n2, n3) = match j {
        Judgment::Arithmetic { n1, n2, n3, .. } => (n1, n2, n3),
        _ => unreachable!(),
    };

    let conclusion = format!("{}", j);
    match n1 {
        // Rule P-Zero: Z plus n is n
        Nat::Z => {
            if n2 == n3 {
                Ok(Derivation {
                    conclusion,
                    rule: "P-Zero".to_string(),
                    premises: vec![],
                })
            } else {
                Err(format!("Cannot prove with P-Zero, since {} != {}", n2, n3))
            }
        }
        // Rule P-SUCC: S(n1) plus n2 is S(n)
        Nat::S(n1_inner) => {
            if let Nat::S(n3_inner) = n3 {
                let premise_j = Judgment::Arithmetic {
                    op: ArithmeticOp::Plus,
                    n1: *n1_inner.clone(),
                    n2: n2.clone(),
                    n3: *n3_inner.clone(),
                };
                let premise_deriv = derive_plus(&premise_j)?;
                Ok(Derivation {
                    conclusion,
                    rule: "P-Succ".to_string(),
                    premises: vec![premise_deriv],
                })
            } else {
                Err(format!("Cannot apply P-Succ, result {} is not a Successor", n3))
            }
        }
    }
}

/// Attempts to prove a "times" judgment.
fn derive_times(j: &Judgment) -> Result<Derivation, String> {
    let (n1, n2, n3) = match j {
        Judgment::Arithmetic { n1, n2, n3, .. } => (n1, n2, n3),
        _ => unreachable!(),
    };

    let conclusion = format!("{}", j);
    match n1 {
        // Rule T-ZERO: Z times n is Z
        Nat::Z => {
            if let Nat::Z = n3 {
                Ok(Derivation {
                    conclusion,
                    rule: "T-Zero".to_string(),
                    premises: vec![],
                })
            } else {
                Err(format!("Cannot prove with T-ZERO, result {} is not Z", n3))
            }
        }
        // Rule T-SUCC: S(n1) times n2 is n4 where n1*n2=n3 and n2+n3=n4
        Nat::S(n1_inner) => {
            let n3_premise = n1_inner.times(n2);
            let prem1_j = Judgment::Arithmetic {
                op: ArithmeticOp::Times,
                n1: *n1_inner.clone(),
                n2: n2.clone(),
                n3: n3_premise.clone(),
            };
            let prem2_j = Judgment::Arithmetic {
                op: ArithmeticOp::Plus,
                n1: n2.clone(),
                n2: n3_premise,
                n3: n3.clone(),
            };

            let prem1_deriv = derive_times(&prem1_j)?;
            let prem2_deriv = derive_plus(&prem2_j)?;

            Ok(Derivation {
                conclusion,
                rule: "T-Succ".to_string(),
                premises: vec![prem1_deriv, prem2_deriv],
            })
        }
    }
}

/// Derivator for CompareNat1: L-Succ and L-Trans
fn derive_compare_v1(j: &Judgment) -> Result<Derivation, String> {
    let (n1, n2) = match j { Judgment::Comparison { n1, n2 } => (n1, n2), _ => unreachable!() };
    if !n1.is_less_than(n2) { return Err(format!("The judgment '{}' is false.", j)); }
    let conclusion = format!("{}", j);

    if let Nat::S(n2_inner) = n2 { if n2_inner.as_ref() == n1 {
        return Ok(Derivation { conclusion, rule: "L-Succ".to_string(), premises: vec![] });
    }}

    let intermediate = Nat::S(Box::new(n1.clone()));
    let prem1_j = Judgment::Comparison { n1: n1.clone(), n2: intermediate.clone() };
    let prem2_j = Judgment::Comparison { n1: intermediate, n2: n2.clone() };
    let prem1_d = derive_compare_v1(&prem1_j)?;
    let prem2_d = derive_compare_v1(&prem2_j)?;
    Ok(Derivation { conclusion, rule: "L-Trans".to_string(), premises: vec![prem1_d, prem2_d] })
}

/// Derivator for CompareNat2: L-Zero and L-SuccSucc
fn derive_compare_v2(j: &Judgment) -> Result<Derivation, String> {
    let (n1, n2) = match j { Judgment::Comparison { n1, n2 } => (n1, n2), _ => unreachable!() };
    if !n1.is_less_than(n2) { return Err(format!("The judgment '{}' is false.", j)); }
    let conclusion = format!("{}", j);

    if let Nat::Z = n1 { if let Nat::S(_) = n2 {
        return Ok(Derivation { conclusion, rule: "L-Zero".to_string(), premises: vec![] });
    }}

    if let (Nat::S(n1_inner), Nat::S(n2_inner)) = (n1, n2) {
        let premise_j = Judgment::Comparison { n1: *n1_inner.clone(), n2: *n2_inner.clone() };
        let premise_deriv = derive_compare_v2(&premise_j)?;
        Ok(Derivation { conclusion, rule: "L-SuccSucc".to_string(), premises: vec![premise_deriv] })
    } else {
        Err(format!("Could not apply L-Zero or L-SuccSucc to prove '{}'", j))
    }
}

/// Derivator for CompareNat3: L-Succ and L-SuccR
fn derive_compare_v3(j: &Judgment) -> Result<Derivation, String> {
    let (n1, n2) = match j { Judgment::Comparison { n1, n2 } => (n1, n2), _ => unreachable!() };
    if !n1.is_less_than(n2) { return Err(format!("The judgment '{}' is false.", j)); }
    let conclusion = format!("{}", j);

    if let Nat::S(n2_inner) = n2 { if n2_inner.as_ref() == n1 {
        return Ok(Derivation { conclusion, rule: "L-Succ".to_string(), premises: vec![] });
    }}

    if let Nat::S(n2_inner) = n2 {
        let premise_j = Judgment::Comparison { n1: n1.clone(), n2: *n2_inner.clone() };
        let premise_deriv = derive_compare_v3(&premise_j)?;
        Ok(Derivation { conclusion, rule: "L-SuccR".to_string(), premises: vec![premise_deriv] })
    } else {
        Err(format!("Could not apply L-Succ or L-SuccR to prove '{}'", j))
    }
}

fn derive_eval(exp: &Expr, n: &Nat) -> Result<Derivation, String> {
    // Correctness check
    if eval_expr(exp) != *n {
        return Err(format!("The judgment '{}' is false.", Judgment::Evaluation{exp: exp.clone(), n: n.clone()}));
    }

    let conclusion = format!("{}", Judgment::Evaluation{exp: exp.clone(), n: n.clone()});
    match exp {
        // Now matches on Expr variants
        Expr::N(_) => Ok(Derivation {
            conclusion,
            rule: "E-Const".to_string(),
            premises: vec![],
        }),
        Expr::Plus(e1, e2) => {
            let n1 = eval_expr(e1);
            let n2 = eval_expr(e2);
            let premise1 = derive_eval(e1, &n1)?;
            let premise2 = derive_eval(e2, &n2)?;
            let premise3_j = Judgment::Arithmetic { op: ArithmeticOp::Plus, n1, n2, n3: n.clone() };
            let premise3 = derive_plus(&premise3_j)?;
            Ok(Derivation { conclusion, rule: "E-Plus".to_string(), premises: vec![premise1, premise2, premise3] })
        }
        Expr::Times(e1, e2) => {
            let n1 = eval_expr(e1);
            let n2 = eval_expr(e2);
            let premise1 = derive_eval(e1, &n1)?;
            let premise2 = derive_eval(e2, &n2)?;
            let premise3_j = Judgment::Arithmetic { op: ArithmeticOp::Times, n1, n2, n3: n.clone() };
            let premise3 = derive_times(&premise3_j)?;
            Ok(Derivation { conclusion, rule: "E-Times".to_string(), premises: vec![premise1, premise2, premise3] })
        }
    }
}
