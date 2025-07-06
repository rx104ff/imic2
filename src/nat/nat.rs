use crate::common::ast::{ArithmeticOp, Judgment, Nat, Expr, ReductionType};
use crate::nat::version::{ComparisonMode};
use crate::nat::proof::Derivation;

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
        Judgment::Reduction { r_type, e1, e2 } => {
            match r_type {
                ReductionType::Multi => derive_multi_step(e1, e2),
                ReductionType::Single => derive_single_step(e1, e2),
                ReductionType::Direct => derive_direct_step(e1, e2),
            }
        }
        _ => Err("This judgment type is not supported by the type checker.".to_string()),
    }
}

fn eval_expr(exp: &Expr) -> Result<Nat, String> {
    match exp {
        Expr::Nat(n) => Ok(n.clone()),
        Expr::Plus(e1, e2) => Ok(eval_expr(e1)?.plus(&eval_expr(e2)?)),
        Expr::Times(e1, e2) => Ok(eval_expr(e1)?.times(&eval_expr(e2)?)),
        _ => Err("This judgment type is not supported by the type checker.".to_string())
    }
}

// Type-safe helper to get the resulting expression from a reduction's conclusion.
fn get_conclusion_expr(j: &Judgment) -> Expr {
    match j {
        Judgment::Reduction { e2, .. } => e2.clone(),
        _ => unreachable!("This helper should only be called on reduction judgments."),
    }
}

fn derive_single_step(e1: &Expr, e2: &Expr) -> Result<Derivation, String> {
    if let Some(derivation) = find_r_step(e1) {
        let derived_e2 = get_conclusion_expr(&derivation.conclusion);
        if derived_e2 == *e2 { Ok(derivation) }
        else { Err(format!("'{}' is not the correct single-step reduction from '{}'. The correct next step is '{}'", e2, e1, derived_e2)) }
    } else { Err(format!("Cannot reduce expression '{}' because it is already a value.", e1)) }
}

fn derive_direct_step(e1: &Expr, e2: &Expr) -> Result<Derivation, String> {
    if let Some(derivation) = find_dr_step(e1) {
        let derived_e2 = get_conclusion_expr(&derivation.conclusion);
        if derived_e2 == *e2 { Ok(derivation) }
        else { Err(format!("'{}' is not the correct direct reduction from '{}'. The correct next step is '{}'", e2, e1, derived_e2)) }
    } else { Err(format!("Cannot directly reduce expression '{}'.", e1)) }
}

fn derive_multi_step(e1: &Expr, e2: &Expr) -> Result<Derivation, String> {
    // The conclusion for the judgment we are currently trying to prove.
    let conclusion = Judgment::Reduction { r_type: ReductionType::Multi, e1: e1.clone(), e2: e2.clone() };

    // Base Case: MR-Zero. An expression reduces to itself in zero steps.
    if e1 == e2 {
        return Ok(Derivation {
            conclusion,
            rule: "MR-Zero".to_string(),
            premises: vec![],
        });
    }

    // Find the very next single step using the existing R-* rules.
    if let Some(single_step_derivation) = find_r_step(e1) {
        let e_prime = get_conclusion_expr(&single_step_derivation.conclusion);

        // This is the proof for the first part of the chain: `e1 -*-> e_prime`
        // We prove it using the MR-One rule, which takes a single-step proof as its premise.
        let premise1 = Derivation {
            conclusion: Judgment::Reduction { r_type: ReductionType::Multi, e1: e1.clone(), e2: e_prime.clone() },
            rule: "MR-One".to_string(),
            premises: vec![single_step_derivation],
        };

        // If that one step was all that was needed to reach the goal, then this is our proof.
        if e_prime == *e2 {
            return Ok(premise1);
        }

        // If more steps are needed, we recursively prove the rest of the chain: `e_prime -*-> e2`
        let premise2 = derive_multi_step(&e_prime, e2)?;

        // The final proof combines the first step and the rest of the steps using MR-Multi.
        return Ok(Derivation {
            conclusion,
            rule: "MR-Multi".to_string(),
            premises: vec![premise1, premise2],
        });
    }

    // If no single step can be taken from e1, and e1 != e2, the judgment is unprovable.
    Err(format!("Cannot reduce '{}' any further, but it does not equal target '{}'.", e1, e2))
}

fn find_r_step(e: &Expr) -> Option<Derivation> {
    match e {
        Expr::Nat(_) => None, // It's a value, cannot be reduced.
        Expr::Plus(l, r) => {
            // Rule R-PlusR: Try to reduce the right side FIRST.
            if let Some(premise) = find_r_step(r) {
                let r_prime = get_conclusion_expr(&premise.conclusion);
                let conclusion = Judgment::Reduction { r_type: ReductionType::Single, e1: e.clone(), e2: Expr::Plus(l.clone(), Box::new(r_prime)) };
                return Some(Derivation { conclusion, rule: "R-PlusR".to_string(), premises: vec![premise] });
            }
            // Rule R-PlusL: ONLY if the right side is a value, try to reduce the left side.
            if let Some(premise) = find_r_step(l) {
                let l_prime = get_conclusion_expr(&premise.conclusion);
                let conclusion = Judgment::Reduction { r_type: ReductionType::Single, e1: e.clone(), e2: Expr::Plus(Box::new(l_prime), r.clone()) };
                return Some(Derivation { conclusion, rule: "R-PlusL".to_string(), premises: vec![premise] });
            }
            // Rule R-Plus: ONLY if both sides are values.
            if let (Expr::Nat(n1), Expr::Nat(n2)) = (l.as_ref(), r.as_ref()) {
                if let Ok(premise) = derive_plus(&Judgment::Arithmetic { op: ArithmeticOp::Plus, n1: n1.clone(), n2: n2.clone(), n3: n1.plus(n2) }) {
                    let conclusion = Judgment::Reduction { r_type: ReductionType::Single, e1: e.clone(), e2: Expr::Nat(n1.plus(n2)) };
                    return Some(Derivation { conclusion, rule: "R-Plus".to_string(), premises: vec![premise] });
                }
            }
            None
        }
        Expr::Times(l, r) => {
            // Assume Times also follows a right-to-left order for consistency.
            // R-TimesR
            if let Some(premise) = find_r_step(r) {
                let r_prime = get_conclusion_expr(&premise.conclusion);
                let conclusion = Judgment::Reduction { r_type: ReductionType::Single, e1: e.clone(), e2: Expr::Times(l.clone(), Box::new(r_prime)) };
                return Some(Derivation { conclusion, rule: "R-TimesR".to_string(), premises: vec![premise] });
            }
            // R-TimesL
            if let Some(premise) = find_r_step(l) {
                let l_prime = get_conclusion_expr(&premise.conclusion);
                let conclusion = Judgment::Reduction { r_type: ReductionType::Single, e1: e.clone(), e2: Expr::Times(Box::new(l_prime), r.clone()) };
                return Some(Derivation { conclusion, rule: "R-TimesL".to_string(), premises: vec![premise] });
            }
            // R-Times
            if let (Expr::Nat(n1), Expr::Nat(n2)) = (l.as_ref(), r.as_ref()) {
                if let Ok(premise) = derive_times(&Judgment::Arithmetic { op: ArithmeticOp::Times, n1: n1.clone(), n2: n2.clone(), n3: n1.times(n2) }) {
                    let conclusion = Judgment::Reduction { r_type: ReductionType::Single, e1: e.clone(), e2: Expr::Nat(n1.times(n2)) };
                    return Some(Derivation { conclusion, rule: "R-Times".to_string(), premises: vec![premise] });
                }
            }
            None
        }
        _ => None
    }
}

/// Helper to find the next step using the DR-* rules (for -d->).
fn find_dr_step(e: &Expr) -> Option<Derivation> {
    match e {
        Expr::Nat(_) => None,
        Expr::Plus(e1, e2) => {
            // Rule DR-PlusL: If the left side is not a value, try to reduce it.
            if !matches!(e1.as_ref(), Expr::Nat(_)) {
                if let Some(premise) = find_dr_step(e1) {
                    let e1_prime = match &premise.conclusion {
                        Judgment::Reduction { e2, .. } => e2.clone(),
                        _ => unreachable!(),
                    };
                    let conclusion_expr = Expr::Plus(Box::new(e1_prime), e2.clone());
                    let conclusion = Judgment::Reduction { r_type: ReductionType::Direct, e1: e.clone(), e2: conclusion_expr };
                    return Some(Derivation {
                        conclusion,
                        rule: "DR-PlusL".to_string(),
                        premises: vec![premise],
                    });
                }
            }

            // Rule DR-PlusR: If left is a value, but right can be reduced, reduce it.
            if let (Expr::Nat(_), Some(premise)) = (e1.as_ref(), find_dr_step(e2)) {
                let e2_prime = match &premise.conclusion {
                    Judgment::Reduction { e2, .. } => e2.clone(),
                    _ => unreachable!(),
                };
                let conclusion_expr = Expr::Plus(e1.clone(), Box::new(e2_prime));
                let conclusion = Judgment::Reduction { r_type: ReductionType::Direct, e1: e.clone(), e2: conclusion_expr };
                return Some(Derivation {
                    conclusion,
                    rule: "DR-PlusR".to_string(),
                    premises: vec![premise],
                });
            }

            // Rule DR-Plus: If both sides are values, perform the addition.
            if let (Expr::Nat(n1), Expr::Nat(n2)) = (e1.as_ref(), e2.as_ref()) {
                let n3 = n1.plus(n2);
                let premise_j = Judgment::Arithmetic { op: ArithmeticOp::Plus, n1: n1.clone(), n2: n2.clone(), n3: n3.clone() };
                if let Ok(premise) = derive_plus(&premise_j) {
                    let conclusion_expr = Expr::Nat(n3);
                    let conclusion = Judgment::Reduction { r_type: ReductionType::Direct, e1: e.clone(), e2: conclusion_expr };
                    return Some(Derivation {
                        conclusion,
                        rule: "DR-Plus".to_string(),
                        premises: vec![premise],
                    });
                }
            }
            
            None
        }
        Expr::Times(e1, e2) => {
            // Rule DR-TimesL: If the left side is not a value, try to reduce it.
            if !matches!(e1.as_ref(), Expr::Nat(_)) {
                if let Some(premise) = find_dr_step(e1) {
                    let e1_prime = match &premise.conclusion {
                        Judgment::Reduction { e2, .. } => e2.clone(),
                        _ => unreachable!(),
                    };
                    let conclusion_expr = Expr::Times(Box::new(e1_prime), e2.clone());
                    let conclusion = Judgment::Reduction { r_type: ReductionType::Direct, e1: e.clone(), e2: conclusion_expr };
                    return Some(Derivation {
                        conclusion,
                        rule: "DR-TimesL".to_string(),
                        premises: vec![premise],
                    });
                }
            }

            // Rule DR-TimesR: If left is a value, but right can be reduced, reduce it.
            if let (Expr::Nat(_), Some(premise)) = (e1.as_ref(), find_dr_step(e2)) {
                let e2_prime = match &premise.conclusion {
                    Judgment::Reduction { e2, .. } => e2.clone(),
                    _ => unreachable!(),
                };
                let conclusion_expr = Expr::Times(e1.clone(), Box::new(e2_prime));
                let conclusion = Judgment::Reduction { r_type: ReductionType::Direct, e1: e.clone(), e2: conclusion_expr };
                return Some(Derivation {
                    conclusion,
                    rule: "DR-TimesR".to_string(),
                    premises: vec![premise],
                });
            }

            // Rule DR-Times: If both sides are values, perform the multiplication.
            if let (Expr::Nat(n1), Expr::Nat(n2)) = (e1.as_ref(), e2.as_ref()) {
                let n3 = n1.times(n2);
                let premise_j = Judgment::Arithmetic { op: ArithmeticOp::Times, n1: n1.clone(), n2: n2.clone(), n3: n3.clone() };
                if let Ok(premise) = derive_times(&premise_j) {
                    let conclusion_expr = Expr::Nat(n3);
                    let conclusion = Judgment::Reduction { r_type: ReductionType::Direct, e1: e.clone(), e2: conclusion_expr };
                    return Some(Derivation {
                        conclusion,
                        rule: "DR-Times".to_string(),
                        premises: vec![premise],
                    });
                }
            }
            None
        }
        _ => None
    }
}

/// Attempts to prove a "plus" judgment.
fn derive_plus(j: &Judgment) -> Result<Derivation, String> {
    let (n1, n2, n3) = match j {
        Judgment::Arithmetic { n1, n2, n3, .. } => (n1, n2, n3),
        _ => unreachable!(),
    };

    match n1 {
        // Rule P-Zero: Z plus n is n
        Nat::Z => {
            if n2 == n3 {
                Ok(Derivation {
                    conclusion: j.clone(),
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
                    conclusion: j.clone(),
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

    match n1 {
        // Rule T-Zero: Z times n is Z
        Nat::Z => {
            if let Nat::Z = n3 {
                Ok(Derivation {
                    conclusion: j.clone(),
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
                conclusion: j.clone(),
                rule: "T-Succ".to_string(),
                premises: vec![prem1_deriv, prem2_deriv],
            })
        }
    }
}

/// Derivator for CompareNat1: L-Succ and L-Trans
fn derive_compare_v1(j: &Judgment) -> Result<Derivation, String> {
    let (n1, n2) = match j { Judgment::Comparison { n1, n2 } => (n1, n2), _ => unreachable!() };
    // if !n1.is_less_than(n2) { return Err(format!("The judgment '{}' is false.", j)); }

    if let Nat::S(n2_inner) = n2 { if n2_inner.as_ref() == n1 {
        return Ok(Derivation { 
            conclusion: j.clone(), 
            rule: "L-Succ".to_string(), 
            premises: vec![] });
    }}

    let intermediate = Nat::S(Box::new(n1.clone()));
    let prem1_j = Judgment::Comparison { n1: n1.clone(), n2: intermediate.clone() };
    let prem2_j = Judgment::Comparison { n1: intermediate, n2: n2.clone() };
    let prem1_d = derive_compare_v1(&prem1_j)?;
    let prem2_d = derive_compare_v1(&prem2_j)?;
    Ok(Derivation { 
        conclusion: j.clone(), 
        rule: "L-Trans".to_string(), 
        premises: vec![prem1_d, prem2_d] })
}

/// Derivator for CompareNat2: L-Zero and L-SuccSucc
fn derive_compare_v2(j: &Judgment) -> Result<Derivation, String> {
    let (n1, n2) = match j { Judgment::Comparison { n1, n2 } => (n1, n2), _ => unreachable!() };
    // if !n1.is_less_than(n2) { return Err(format!("The judgment '{}' is false.", j)); }

    if let Nat::Z = n1 { if let Nat::S(_) = n2 {
        return Ok(Derivation { 
            conclusion: j.clone(),
            rule: "L-Zero".to_string(), 
            premises: vec![] 
        });
    }}

    if let (Nat::S(n1_inner), Nat::S(n2_inner)) = (n1, n2) {
        let premise_j = Judgment::Comparison { n1: *n1_inner.clone(), n2: *n2_inner.clone() };
        let premise_deriv = derive_compare_v2(&premise_j)?;
        Ok(Derivation { 
            conclusion: j.clone(),
            rule: "L-SuccSucc".to_string(), 
            premises: vec![premise_deriv] })
    } else {
        Err(format!("Could not apply L-Zero or L-SuccSucc to prove"))
    }
}

/// Derivator for CompareNat3: L-Succ and L-SuccR
fn derive_compare_v3(j: &Judgment) -> Result<Derivation, String> {
    let (n1, n2) = match j { Judgment::Comparison { n1, n2 } => (n1, n2), _ => unreachable!() };
    // if !n1.is_less_than(n2) { return Err(format!("The judgment '{}' is false.", j)); }

    if let Nat::S(n2_inner) = n2 { if n2_inner.as_ref() == n1 {
        return Ok(Derivation { 
            conclusion: j.clone(),
            rule: "L-Succ".to_string(), 
            premises: vec![] });
    }}

    if let Nat::S(n2_inner) = n2 {
        let premise_j = Judgment::Comparison { n1: n1.clone(), n2: *n2_inner.clone() };
        let premise_deriv = derive_compare_v3(&premise_j)?;
        Ok(Derivation { 
            conclusion: j.clone(),
            rule: "L-SuccR".to_string(), 
            premises: vec![premise_deriv] 
        })
    } else {
        Err(format!("Could not apply L-Succ or L-SuccR to prove"))
    }
}

fn derive_eval(exp: &Expr, n: &Nat) -> Result<Derivation, String> {
    match exp {
        // Now matches on Expr variants
        Expr::Nat(_) => Ok(Derivation {
            conclusion: Judgment::Evaluation{exp: exp.clone(), n: n.clone()},
            rule: "E-Const".to_string(),
            premises: vec![],
        }),
        Expr::Plus(e1, e2) => {
            let n1 = eval_expr(e1)?;
            let n2 = eval_expr(e2)?;
            let premise1 = derive_eval(e1, &n1)?;
            let premise2 = derive_eval(e2, &n2)?;
            let premise3_j = Judgment::Arithmetic { op: ArithmeticOp::Plus, n1, n2, n3: n.clone() };
            let premise3 = derive_plus(&premise3_j)?;
            Ok(Derivation { 
                conclusion: Judgment::Evaluation{exp: exp.clone(), n: n.clone()},
                rule: "E-Plus".to_string(), 
                premises: vec![premise1, premise2, premise3] })
        }
        Expr::Times(e1, e2) => {
            let n1 = eval_expr(e1)?;
            let n2 = eval_expr(e2)?;
            let premise1 = derive_eval(e1, &n1)?;
            let premise2 = derive_eval(e2, &n2)?;
            let premise3_j = Judgment::Arithmetic { op: ArithmeticOp::Times, n1, n2, n3: n.clone() };
            let premise3 = derive_times(&premise3_j)?;
            Ok(Derivation { 
                conclusion: Judgment::Evaluation{exp: exp.clone(), n: n.clone()},
                rule: "E-Times".to_string(),
                premises: vec![premise1, premise2, premise3]
            })
        },
         _ => Err("This judgment type is not supported by the type checker.".to_string()),
    }
}
