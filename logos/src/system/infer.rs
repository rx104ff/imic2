// --- PROVIDER DEFINITIONS ---

use std::collections::BTreeMap;

use crate::{common::{ast::{core::{NamedVar, Op, Variable}, expr::Expr, r#type::{IsIntType, Type, TypeVar}}, proof::{Derivation, Judgment}, unifier::Substitution}, system::{core::{Axiom, Rule}, judgment_traits::{HasExpr, HasResult}}};

pub use logos_proc::define_system;


// --- Type Aliases for Clarity ---
pub type InferJudgment = Judgment<NamedVar, Type<NamedVar>, Type<NamedVar>>;
pub type InferDerivation = Derivation<InferJudgment>;

// --- State for the Inference Process ---
#[derive(Clone)]
pub struct InferContext {
    pub sub: Substitution,
    var_counter: usize,
}

impl InferContext {
    pub fn new() -> Self {
        Self { sub: BTreeMap::new(), var_counter: 0 }
    }
    pub fn create_fresh_type_var<V: Variable>(&mut self) -> Type<V> {
        let name = format!("'{}", ((self.var_counter % 26) as u8 + b'a') as char);
        let id = self.var_counter;
        self.var_counter += 1;
        Type::Var(TypeVar { id: id + 1000, name })
    }
}

pub struct TIntRule;
impl<J, C> Rule<J, C> for TIntRule
where
    J: HasExpr + HasResult,
    J::R: IsIntType, // e.g. J::R can be `Type<NamedVar>` and this will work
{
    fn apply(&self, _ctx: &mut C, j: &J) -> Option<Result<(String, Vec<J>), String>> {
        if let Expr::Int(_) = j.expr() {
            // This comparison now works generically thanks to the new `PartialEq` impl.
            if j.result().is_int() {
                return Some(Ok(("T-Int".to_string(), vec![])));
            }
        }
        None
    }
}


// RULE: Provides the T-Plus rule, which has recursive premises.
pub struct TPlusRule;
impl Rule<InferJudgment, InferContext> for TPlusRule {
    fn apply(&self, _ctx: &mut InferContext, j: &InferJudgment) -> Option<Result<(String, Vec<InferJudgment>), String>> {
        if let Expr::BinOp(e1, Op::Add, e2) = &j.expr {
            let premises = vec![
                Judgment { env: j.env.clone(), expr: *e1.clone(), result: Type::Int },
                Judgment { env: j.env.clone(), expr: *e2.clone(), result: Type::Int },
            ];
            if j.result != Type::Int {
                return Some(Err("Result of '+' must be Int".to_string()));
            }
            return Some(Ok(("T-Plus".to_string(), premises)));
        }
        None
    }
}

// AXIOM: Provides the B-Plus axiom, which is a terminal proof component.
pub struct BPlusAxiom;
impl Axiom<InferJudgment, InferContext> for BPlusAxiom {
     fn axiom(&self, _ctx: &mut InferContext, j: &InferJudgment) -> Option<Result<InferDerivation, String>> {
        if let Expr::BinOp(_, Op::Add, _) = &j.expr {
             let axiom_judgment = Judgment { env: j.env.clone(), expr: j.expr.clone(), result: Type::Int };
             return Some(Ok(Derivation { judgment: axiom_judgment, rule: "B-Plus".to_string(), premises: vec![] }));
        }
        None
    }
}


// --- SYSTEM ASSEMBLY ---


define_system! {
    pub struct InferenceSystem {
        rule TIntRule;
        rule TPlusRule;
    },
    judgment: InferJudgment,
    context: InferContext
}


// impl InferenceSystem {
//     pub fn new() -> Self {
//         Self {
//             providers: (
//                 (TIntRule, None),
//                 (TPlusRule, None),
//             ),
//         }
//     }
// }

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{common::ast::env::Env, system::core::System};

    #[test]
    fn test_derive_one_plus_two() {
        // Setup
        let system = InferenceSystem::new();
        let mut context = InferContext::new();

        // The expression we want to find the type of: 1 + 2
        let expr = Expr::BinOp(
            Box::new(Expr::Int(1)),
            Op::Add,
            Box::new(Expr::Int(2)),
        );

        // The judgment we want to prove: "In an empty environment, 1 + 2 has type Int"
        let judgment_to_prove = Judgment {
            env: Env(vec![]),
            expr: expr,
            result: Type::Int,
        };

        // Execution
        let result = system.derive(&mut context, &judgment_to_prove);

        // Verification
        assert!(result.is_ok(), "Derivation should succeed for 1 + 2");
        
        let derivation = result.unwrap();
        println!("--- DERIVATION SUCCESSFUL ---");
        println!("{}\n", derivation);

        // Check the structure of the proof tree
        assert_eq!(derivation.rule, "T-Plus");
        assert_eq!(derivation.premises.len(), 2); // 2 recursive premises, 1 axiom
        assert_eq!(derivation.premises[0].rule, "T-Int");
        assert_eq!(derivation.premises[1].rule, "T-Int");
    }
}


// /// Correct instantiation and usage example.
// fn main_example() {
//     let system = InferenceSystem {
//         providers: (
//             // For `rule TIntRule;`
//             (TIntRule, None),
//             // For `rule TPlusRule => axiom BPlusAxiom;`
//             (TPlusRule, Some(BPlusAxiom)),
//         ),
//     };

//     // ... setup and call system.derive ...
// }