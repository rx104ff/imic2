#[macro_export]
macro_rules! build_expression_parser {
    (
        parser = $parser_struct:ty,
        var_type = $var:ty,
        primitive_parsers: [ $( $primitive_trait:ident ),* ],
        unary_parsers : [ $( $unary_trait:ident ),* ],
        dispatch_parsers: [ $( $dispatch_trait:ident ),* ],
        binop_chain: [ { $first_trait_in_chain:ident $(, $_rest:ident)* } $(, $rest_chain_entry:tt)* ]
    ) => {

        // Phase 1: Implement the primitive and dispatch traits.
        $(
            impl crate::parser::primitive::$primitive_trait<$crate::common::ast::expr::Expr<$var>> for $parser_struct {}
        )*
        $(
            impl crate::parser::expression::$dispatch_trait<$var> for $parser_struct {}
        )*

        // Phase 2: Kick off the internal recursive macro.
        $crate::__internal_build_parser_logic! {
            parser = $parser_struct,
            var_type = $var,
            primitive_parsers = [ $( $primitive_trait ),* ],
            dispatch_parsers = [ $( $dispatch_trait ),* ],
            unary_parsers = [ $( $unary_trait ),* ],
            first_trait_overall = $first_trait_in_chain,
            methods = {},
            // Start with an empty list of traits
            all_traits = {},
            // Pass the entire reconstructed chain to the internal helper.
            chain = [ { $first_trait_in_chain $(, $_rest)* } $(, $rest_chain_entry)* ]
        }

        // Phase 3: Implement the BaseParser trait.
        impl crate::parser::core::BaseParser for $parser_struct {
            type V = $var;
            fn core(&mut self) -> &mut crate::parser::core::ParserCore {
                &mut self.core
            }
        }
    };
}

#[macro_export]
macro_rules! __internal_build_parser_logic {
    (
        parser = $parser_struct:ty,
        var_type = $var:ty,
        primitive_parsers = [ $( $primitive_trait:ident ),* ],
        dispatch_parsers = [ $( $dispatch_trait:ident ),* ],
        unary_parsers = [ $( $unary_trait:ident ),* ],
        first_trait_overall = $first_trait_overall:ident,
        methods = { $( $methods:tt )* },
        all_traits = { $( $all_traits:ident, )* },
        chain = [
            { $first_current:ident $(, $rest_current:ident)* }, // Current level
            { $first_next:ident $(, $rest_next:ident)* }       // Next level
            $(, $tail:tt )* // The rest
        ]
    ) => {
        paste::paste! {
            // Recurse, adding the new method and traits to the accumulators.
            $crate::__internal_build_parser_logic! {
                parser = $parser_struct,
                var_type = $var,
                primitive_parsers = [ $( $primitive_trait ),* ],
                dispatch_parsers = [ $( $dispatch_trait ),* ],
                unary_parsers = [ $( $unary_trait ),* ],
                first_trait_overall = $first_trait_overall,
                methods = {
                    $( $methods )*

                    // The new method for the current level. It's named after the first trait.
                    fn [<parse_ $first_current:lower _level>] (&mut self) -> Result<$crate::common::ast::expr::Expr<$var>, String> {
                        // It calls the parser for the *next* level of precedence.
                        let mut lhs = self.[<parse_ $first_next:lower _level>]()?;
                        loop {
                            let maybe_op = {
                                let mut op = None;
                                if op.is_none() { 
                                    op = <Self as $crate::parser::expression::$first_current<$var>>::handle(self); 
                                }
                                $( 
                                    if op.is_none() { 
                                        op = <Self as $crate::parser::expression::$rest_current<$var>>::handle(self); 
                                    } 
                                )*
                                op
                            };
                            if let Some(op) = maybe_op {
                                let rhs = if <Self as crate::parser::expression::$first_current<$var>>::is_right_assoc() {
                                    self.[<parse_ $first_current:lower _level>]()?
                                } else {
                                    self.[<parse_ $first_next:lower _level>]()?
                                };
                                lhs = $crate::common::ast::expr::Expr::BinOp(Box::new(lhs), op, Box::new(rhs));
                            } else { break; }
                        }
                        Ok(lhs)
                    }
                },
                all_traits = { $( $all_traits, )* $first_current, $( $rest_current, )* },
                chain = [ { $first_next $(, $rest_next)* } $(, $tail)* ]
            }
        }
    };

    (
        parser = $parser_struct:ty,
        var_type = $var:ty,
        primitive_parsers = [ $( $primitive_trait:ident ),* ],
        dispatch_parsers = [ $( $dispatch_trait:ident ),* ],
        unary_parsers = [ $( $unary_trait:ident ),* ],
        first_trait_overall = $first_trait_overall:ident,
        methods = { $( $methods:tt )* },
        all_traits = { $( $all_traits:ident, )* },
        chain = [ { $first_current:ident $(, $rest_current:ident)* } ]
    ) => {
        // 1. Implement all the collected binop traits
        $(
            impl crate::parser::expression::$all_traits<$var> for $parser_struct {}
        )*
        // Implement the traits from the final level
        impl crate::parser::expression::$first_current<$var> for $parser_struct {}
        $(
            impl crate::parser::expression::$rest_current<$var> for $parser_struct {}
        )*
        $(
            impl crate::parser::primitive::$unary_trait<$crate::common::ast::expr::Expr<$var>> for $parser_struct {}
        )*

        paste::paste! {
            // 2. Generate the single `impl Parser` block with all chained methods.
            impl $parser_struct {
                $( $methods )*

                // The method for the final level of the chain.
                fn [<parse_ $first_current:lower _level>] (&mut self) -> Result<$crate::common::ast::expr::Expr<$var>, String> {
                    // It calls `parse_expr_atom` to terminate the recursion.
                    let mut lhs = self.parse_expr_atom()?;
                    loop {
                        let maybe_op = {
                            let mut op = None;
                            if op.is_none() { op = <Self as crate::parser::expression::$first_current<$var>>::handle(self); }
                            $( if op.is_none() { op = <Self as crate::parser::expression::$rest_current<$var>>::handle(self); } )*
                            op
                        };
                        if let Some(op) = maybe_op {
                            let rhs = if <Self as crate::parser::expression::$first_current<$var>>::is_right_assoc() {
                                self.[<parse_ $first_current:lower _level>]()?
                            } else {
                                self.parse_expr_atom()?
                            };
                            lhs = $crate::common::ast::expr::Expr::BinOp(Box::new(lhs), op, Box::new(rhs));
                        } else { break; }
                    }
                    Ok(lhs)
                }

                fn parse_unary(&mut self) -> Result<$crate::common::ast::expr::Expr<$var>, String> {
                    if let Some(token) = self.core().peek() {
                        $(
                            if <Self as crate::parser::primitive::$unary_trait<$crate::common::ast::expr::Expr<$var>>>::check(token) {
                                return <Self as crate::parser::primitive::$unary_trait<$crate::common::ast::expr::Expr<$var>>>::parse(self);
                            }
                        )*
                    }
                    self.parse_expr_atom()
                }
            }
        }

        // 3. Implement the `ExpressionParser` trait.
        impl crate::parser::expression::traits::ExpressionParser<$var> for $parser_struct {
            fn parse_expr(&mut self) -> Result<$crate::common::ast::expr::Expr<$var>, String> {
                if let Some(token) = self.core().peek() {
                    $(
                        if <Self as crate::parser::expression::$dispatch_trait<$var>>::check(token) {
                            return <Self as crate::parser::expression::$dispatch_trait<$var>>::parse(self);
                        }
                    )*
                }
                // Call the generated method for the highest precedence level.
                paste::paste! {
                    //println!("{}", stringify!($first_trait_overall));
                    self.[<parse_ $first_trait_overall:lower _level>]()
                }
            }

            fn parse_expr_atom(&mut self) -> Result<$crate::common::ast::expr::Expr<$var>, String> {
                if let Some(token) = self.core().peek().cloned() {
                    $(
                        if <Self as crate::parser::primitive::$primitive_trait<$crate::common::ast::expr::Expr<$var>>>::check(&token) {
                            return <Self as crate::parser::primitive::$primitive_trait<$crate::common::ast::expr::Expr<$var>>>::parse(self);
                        }
                    )*
                    $(
                        if <Self as crate::parser::primitive::$unary_trait<$crate::common::ast::expr::Expr<$var>>>::check(&token) {
                        return self.parse_unary();
                        }
                    )*
                    $(
                        if <Self as crate::parser::expression::$dispatch_trait<$var>>::check(&token) {
                            return <Self as crate::parser::expression::$dispatch_trait<$var>>::parse(self);
                        }
                    )*
        
                }
                Err(format!("Unexpected token at atomic level: {:?}", self.core().peek()))
            }
        }
    };
}
