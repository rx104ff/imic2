#[macro_export]
macro_rules! define_system_try_rules {
    // Base Case 1: The list of rules is empty.
    (
        $self:ident, 
        $ctx:ident, 
        $judgment:ident, 
        $index:expr, 
    ) => {
        Err(format!("No rule found to derive the judgment for: {:?}", $judgment))
    };

    // Base Case 2: Exactly one rule is left.
    (
        $self:ident, 
        $ctx:ident, 
        $judgment:ident, 
        $index:expr, 
        $head_rule:ty
    ) => {
        {
            let (rule_provider, axiom_provider_opt) = &$self.providers.$index;
            if let Some(rule_result) = rule_provider.apply($ctx, $judgment) {
                let (rule_name, recursive_premises) = rule_result?;
                let mut derived_premises = Vec::new();

                for premise_judgment in recursive_premises {
                    derived_premises.push($self.derive($ctx, &premise_judgment)?);
                }
                if let Some(axiom_provider) = axiom_provider_opt {
                    if let Some(axiom_result) = axiom_provider.axiom($ctx, $judgment) {
                        derived_premises.push(axiom_result?);
                    }
                }
                return Ok($crate::common::proof::Derivation {
                    judgment: $judgment.clone(),
                    rule: rule_name,
                    premises: derived_premises,
                });
            }
            Err(format!("No rule found to derive the judgment for: {:?}", $judgment))
        }
    };

    // Recursive Step: One rule at the head, one or more in the tail.
    (
        $self:ident, 
        $ctx:ident, 
        $judgment:ident, 
        $index:expr, 
        $head_rule:ty, 
        $($tail_rules:ty),+
    ) => {
        {
            let (rule_provider, axiom_provider_opt) = &$self.providers.$index;
            if let Some(rule_result) = rule_provider.apply($ctx, $judgment) {
                let (rule_name, recursive_premises) = rule_result?;
                let mut derived_premises = Vec::new();
                for premise_judgment in recursive_premises {
                    derived_premises.push($self.derive($ctx, &premise_judgment)?);
                }
                if let Some(axiom_provider) = axiom_provider_opt {
                    if let Some(axiom_result) = axiom_provider.axiom($ctx, $judgment) {
                        derived_premises.push(axiom_result?);
                    }
                }
                return Ok($crate::common::proof::Derivation {
                    judgment: $judgment.clone(),
                    rule: rule_name,
                    premises: derived_premises,
                });
            } else {
                let next_index = $index + 1;
                define_system_try_rules!($self, $ctx, $judgment, next_index, $($tail_rules),*)
            }
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __unroll_rules {
    // ---- Internal Workers ----
    // This is the main recursive worker.
    // It takes a literal index and the head/tail of the rule type list.
    (@inner $self:ident, $ctx:ident, $judgment:ident, $index:literal, $head_rule:ty, $($tail_rules:ty),*) => {
        // Generate a code block for the current rule at its literal index.
        {
            let (rule_provider, axiom_provider_opt) = &$self.providers.$index;
            if let Some(rule_result) = rule_provider.apply($ctx, $judgment) {
                let (rule_name, recursive_premises) = rule_result?;
                let mut derived_premises = Vec::new();
                for premise_judgment in recursive_premises {
                    derived_premises.push($self.derive($ctx, &premise_judgment)?);
                }
                if let Some(axiom_provider) = axiom_provider_opt {
                    if let Some(axiom_result) = axiom_provider.axiom($ctx, $judgment) {
                        derived_premises.push(axiom_result?);
                    }
                }
                return Ok($crate::common::proof::Derivation {
                    judgment: $judgment.clone(),
                    rule: rule_name,
                    premises: derived_premises,
                });
            }
        }
        // After trying the current rule, recurse to generate the `else` block for the rest.
        // We calculate the next index and pass it as a literal to the next expansion.
        // NOTE: This arithmetic is happening at compile-time during macro expansion.
        $crate::__unroll_rules!(@inner $self, $ctx, $judgment, {$index + 1}, $($tail_rules),*);
    };

    // Base case: This arm handles the very last rule in the list.
    (@inner $self:ident, $ctx:ident, $judgment:ident, $index:literal, $head_rule:ty) => {
        // It has the same logic, but doesn't generate a recursive `else` block.
        {
            let (rule_provider, axiom_provider_opt) = &$self.providers.$index;
            if let Some(rule_result) = rule_provider.apply($ctx, $judgment) {
                let (rule_name, recursive_premises) = rule_result?;
                let mut derived_premises = Vec::new();
                for premise_judgment in recursive_premises {
                    derived_premises.push($self.derive($ctx, &premise_judgment)?);
                }
                if let Some(axiom_provider) = axiom_provider_opt {
                    if let Some(axiom_result) = axiom_provider.axiom($ctx, $judgment) {
                        derived_premises.push(axiom_result?);
                    }
                }
                return Ok($crate::common::proof::Derivation {
                    judgment: $judgment.clone(),
                    rule: rule_name,
                    premises: derived_premises,
                });
            }
        }
    };

    ($self:ident, $ctx:ident, $judgment:ident, $($rules:ty),*) => {
        // Kick off the unrolling process starting with index 0.
        $crate::__unroll_rules!(@inner $self, $ctx, $judgment, 0, $($rules),*);
        // If the unrolling finishes without returning, it means no rule was found.
        return Err(format!("No rule found to derive the judgment for: {:?}", $judgment));
    };
}

#[macro_export]
macro_rules! define_system {
    (
        pub struct $system_name:ident {
            $(
                rule $rule_provider:ty $(=> axiom $axiom_provider:ty)?;
            )*
        },
        judgment: $judgment_type:ty,
        context: $context_type:ty
    ) => {
        #[macro_export]
        macro_rules! optional_type {
            () => { Option<()> };
            ($ty:ty) => { Option<$ty> };
        }

        pub struct $system_name {
            pub providers: (
                $(
                    ($rule_provider, optional_type!($($axiom_provider)?)),
                )*
            ),
        }

        impl $crate::system::traits::System<$judgment_type, $context_type> for $system_name {
            fn derive(&self, ctx: &mut $context_type, judgment: &$judgment_type) -> Result<$crate::common::proof::Derivation<$judgment_type>, String> {
                $crate::__unroll_rules!(self, ctx, judgment, $($rule_provider),*)
            }
        }
    };
}

