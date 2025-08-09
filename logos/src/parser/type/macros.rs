#[macro_export]
macro_rules! build_type_parser {
    (
        parser = $parser_struct:ty,
        var_type = $var:ty,
        primitive_parsers: [ $( $primitive_trait:ident ),* ],
        postfix_parsers: [ $( $postfix_trait:ident ),* ],
        binop_chain: [ { $first_binop:ident $(, $rest_binops:ident)* } $(, $rest_chain_entry:tt)* ]
        $(, stateful_primitives: [
            $( { base: $stateful_primitive:ident, state_type: $required_state:ty } ),*
        ])?
    ) => {
            paste::paste! {
            pub mod [<$parser_struct:lower _type_impl>] {
                pub fn initialize_states() -> std::collections::HashMap<std::any::TypeId, Box<dyn $crate::parser::primitive::State>> {
                    #[allow(unused_mut)]
                    let mut states: std::collections::HashMap<std::any::TypeId, Box<dyn $crate::parser::primitive::State>> = std::collections::HashMap::new();
                    $($(
                        states.insert(
                            std::any::TypeId::of::<$required_state>(),
                            Box::new(<$required_state>::new())
                        );
                    )*)?
                    states
                }
            }
        }

        $( 
            impl crate::parser::r#type::traits::$postfix_trait<$var> for $parser_struct {} 
        )* 
        
        // Binop traits are implemented via the recursive macro below.

        // --- Implement mandatory primitive traits ---
        $(
            impl crate::parser::primitive::$primitive_trait<crate::common::ast::r#type::Type<$var>> for $parser_struct {}
        )*

        $($(
            // Implement the generic accessor for the required state type.
            impl $crate::parser::primitive::HasState<$required_state> for $parser_struct {
                fn state_mut(&mut self) -> &mut $required_state {
                    self.states
                        .get_mut(&std::any::TypeId::of::<$required_state>())
                        .expect("Required state not found. Did you declare it in the build_type_parser! macro?")
                        .as_any_mut()
                        .downcast_mut::<$required_state>()
                        .expect("Failed to downcast state to required type.")
                }
                fn state(&self) -> &$required_state {
                    self.states
                        .get(&std::any::TypeId::of::<$required_state>())
                        .expect("Required state not found.")
                        .as_any()
                        .downcast_ref::<$required_state>()
                        .expect("Failed to downcast state to required type.")
                }
            }
            // Implement the logic trait itself.
            impl $crate::parser::primitive::$stateful_primitive<$var> for $parser_struct {}
        )*)?

        // --- Generate Parser Methods ---
        $crate::__internal_build_type_parser_logic! {
            parser = $parser_struct,
            var_type = $var,
            primitive_parsers = [ $( $primitive_trait ),* ],
            $(stateful_primitives = [ $( $stateful_primitive ),* ],)?
            postfix_parsers = [ $( $postfix_trait ),* ],
            chain = [ { $first_binop $(, $rest_binops)* } $(, $rest_chain_entry)* ]
        }
    };
}

#[macro_export]
macro_rules! __internal_build_type_parser_logic {
    // Recursive case: More than one level in the chain.
    (
        parser = $parser_struct:ty,
        var_type = $var:ty,
        primitive_parsers = [ $( $primitive_trait:ident ),* ],
        $(stateful_primitives = [ $( $stateful_primitive:ident ),* ],)?
        postfix_parsers = [ $( $postfix_trait:ident ),* ],
        chain = [
            { $first_current:ident $(, $rest_current:ident)* },
            { $next_level:ident $(, $rest_next:ident)* }
            $(, $tail:tt )* 
        ]
    ) => {

        impl crate::parser::r#type::traits::$first_current<$var> for $parser_struct {}
        $( 
            impl crate::parser::r#type::traits::$rest_current<$var> for $parser_struct {} 
        )*

        paste::paste! {
            impl $parser_struct {
                fn [<parse_ $first_current:lower _level>] (&mut self) -> Result<crate::common::ast::Type<$var>, String> {
                    let mut lhs = self.[<parse_ $next_level:lower _level>]()?;
                    loop {
                        let maybe_op = {
                            let mut op = None;
                            if op.is_none() { op = <Self as crate::parser::r#type::traits::$first_current<$var>>::handle(self); }
                            $( if op.is_none() { op = <Self as crate::parser::r#type::traits::$rest_current<$var>>::handle(self); } )*
                            op
                        };

                        if let Some(op) = maybe_op {
                            let rhs = if <Self as crate::parser::r#type::traits::$first_current<$var>>::is_right_assoc() {
                                self.[<parse_ $first_current:lower _level>]()?
                            } else {
                                self.[<parse_ $next_level:lower _level>]()?
                            };
                            lhs = crate::common::ast::Type::BinOp(Box::new(lhs), op, Box::new(rhs));
                        } else { break; }
                    }
                    Ok(lhs)
                }
            }
        }
        // Recurse to build the next level of the parser.
        $crate::__internal_build_type_parser_logic! {
            parser = $parser_struct,
            var_type = $var,
            primitive_parsers = [ $( $primitive_trait ),* ],
            $(postfix_parsers: [ $( $postfix_trait ),* ],)?
            chain = [ { $next_level $(, $rest_next)* } $(, $tail)* ]
        }
    };

    // Base case: The last (or only) level in the chain.
    (
        parser = $parser_struct:ty,
        var_type = $var:ty,
        primitive_parsers = [ $( $primitive_trait:ident ),* ],
        $(stateful_primitives = [ $( $stateful_primitive:ident ),* ],)?
        postfix_parsers = [ $( $postfix_trait:ident ),* ],
        chain = [ { $first_current:ident $(, $rest_current:ident)* } ]
    ) => {
        impl crate::parser::r#type::traits::$first_current<$var> for $parser_struct {}
        $( 
            impl crate::parser::r#type::traits::$rest_current<$var> for $parser_struct {} 
        )*

        paste::paste! {
            impl $parser_struct {
                fn [<parse_ $first_current:lower _level>] (&mut self) -> Result<crate::common::ast::r#type::Type<$var>, String> {
                    let mut lhs = self.parse_type_atom()?;
                    // Postfix logic is handled immediately after parsing an atom.
                        loop {
                            let mut handled = false;
                            if let Some(token) = self.core().peek() {
                                $(
                                    if !handled && <Self as crate::parser::r#type::traits::$postfix_trait<$var>>::check(token) {
                                        lhs = <Self as crate::parser::r#type::traits::$postfix_trait<$var>>::handle(self, lhs)?;
                                        handled = true;
                                    }
                                )*
                            }
                            if !handled { break; }
                        }

                     loop {
                        let maybe_op = {
                            let mut op = None;
                            if op.is_none() { op = <Self as crate::parser::r#type::traits::$first_current<$var>>::handle(self); }
                            $( if op.is_none() { op = <Self as crate::parser::r#type::traits::$rest_current<$var>>::handle(self); } )*
                            op
                        };

                        if let Some(op) = maybe_op {
                            let rhs = if <Self as crate::parser::r#type::traits::$first_current<$var>>::is_right_assoc() {
                                self.[<parse_ $first_current:lower _level>]()?
                            } else {
                                self.parse_type_atom()?
                            };
                            lhs = crate::common::ast::r#type::Type::BinOp(Box::new(lhs), op, Box::new(rhs));
                        } else { break; }
                    }
                    Ok(lhs)
                }
            }
        }

        impl crate::parser::r#type::traits::TypeParser<$var> for $parser_struct {
            fn parse_type(&mut self) -> Result<crate::common::ast::r#type::Type<$var>, String> {
                paste::paste! { self.[<parse_ $first_current:lower _level>]() }
            }
            
            fn parse_type_atom(&mut self) -> Result<crate::common::ast::r#type::Type<$var>, String> {
                if let Some(token) = self.core().peek().cloned() {
                    $(
                        if <Self as crate::parser::primitive::$primitive_trait<crate::common::ast::r#type::Type<$var>>>::check(&token) {
                            return <Self as crate::parser::primitive::$primitive_trait<crate::common::ast::r#type::Type<$var>>>::parse(self);
                        }
                    )*
                    $($(
                        if <Self as $crate::parser::primitive::$stateful_primitive<$var>>::check(&token) {
                            return <Self as $crate::parser::primitive::$stateful_primitive<$var>>::parse(self);
                        }
                    )*)?
                }
                Err(format!("Unexpected token at type atomic level: {:?}", self.core().peek()))
            }
        }
    };
}