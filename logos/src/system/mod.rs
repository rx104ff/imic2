pub mod core;
pub mod macros;
pub mod infer;
pub mod judgment_traits;

pub mod inferrence;
pub mod evaluation;
pub mod reduction;

pub use logos_proc::{define_system, JudgmentTraits};
