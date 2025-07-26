use crate::common::ast::{core::{ArithmeticOp, NamedVar, ReductionType}, expr::{Expr, NamedExpr, NamelessExpr}, nat::Nat, r#type::{MonoTypeEnv, PolyTypeEnv, Type}, value::{NamedEnv, NamelessEnv}};

#[derive(Debug, Clone, PartialEq)]
pub enum Judgment {
   
    // For Nat arithmetic
    Arithmetic { op: ArithmeticOp, n1: Nat, n2: Nat, n3: Nat },

    // For Nat comparison
    Comparison { n1: Nat, n2: Nat },

    // For Nat evaluation
    Evaluation { exp: Expr<NamedVar>, n: Nat },

    // For Nat reduction
    Reduction { r_type: ReductionType, e1: NamedExpr, e2: NamedExpr },

    NamelessEvaluation (NamelessEnv, NamelessExpr),
    
    // For ML evaluation
    EvaluatesTo(NamedEnv, NamedExpr), // Assuming Type can also represent ML values
    
    // For Type Checking
    Infer(MonoTypeEnv, NamedExpr, Type<NamedVar>),
    
    // For Polymorphic Inference
    PolyInfer(PolyTypeEnv, NamedExpr, Type<NamedVar>),
}
