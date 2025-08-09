#[macro_export]
macro_rules! build_value_parser {
    (
        parser = $parser_struct:ty,
        var_type = $var:ty,
        primitive_parsers: [ $( $primitive_trait:ident ),* ],
        dispatch_parsers: [ $( $dispatch_trait:ident ),* ],
        binop_chain: [ { $first_trait_in_chain:ident $(, $_rest:ident)* } $(, $rest_chain_entry:tt)* ]
    ) => {

        $(
            impl crate::parser::primitive::$primitive_trait<crate::common::ast::value::Value<$var>> for $parser_struct {}
        )*
        $(
            impl crate::parser::value::traits::$dispatch_trait<$var> for $parser_struct {}
        )*

        $crate::__internal_build_value_parser_logic! {
            parser = $parser_struct,
            var_type = $var,
            primitive_parsers = [ $( $primitive_trait ),* ],
            dispatch_parsers = [ $( $dispatch_trait ),* ],
            first_trait_overall = $first_trait_in_chain,
            methods = {},
            all_traits = {},
            chain = [ { $first_trait_in_chain $(, $_rest)* } $(, $rest_chain_entry)* ]
        }
    };
}

#[macro_export]
macro_rules! __internal_build_value_parser_logic {
    (
        parser = $parser_struct:ty,
        var_type = $var:ty,
        primitive_parsers = [ $( $primitive_trait:ident ),* ],
        dispatch_parsers = [ $( $dispatch_trait:ident ),* ],
        first_trait_overall = $first_trait_overall:ident,
        methods = { $( $methods:tt )* },
        all_traits = { $( $all_traits:ident, )* },
        chain = [
            { $first_current:ident $(, $rest_current:ident)* },
            { $first_next:ident $(, $rest_next:ident)* }
            $(, $tail:tt )*
        ]
    ) => {
        paste::paste! {
            $crate::__internal_build_value_parser_logic! {
                parser = $parser_struct,
                var_type = $var,
                primitive_parsers = [ $( $primitive_trait ),* ],
                dispatch_parsers = [ $( $dispatch_trait ),* ],
                first_trait_overall = $first_trait_overall,
                methods = {
                    $( $methods )*

                    fn [<parse_ $first_current:lower _level>] (&mut self) -> Result<crate::common::ast::Value<$var>, String> {
                        let mut lhs = self.[<parse_ $first_next:lower _level>]()?;
                        loop {
                            let maybe_op = {
                                let mut op = None;
                                if op.is_none() {
                                    op = <Self as crate::parser::value::traits::$first_current<$var>>::handle(self);
                                }
                                $(
                                    if op.is_none() {
                                        op = <Self as crate::parser::value::traits::$rest_current<$var>>::handle(self);
                                    }
                                )*
                                op
                            };
                            if let Some(op) = maybe_op {
                                let rhs = if <Self as crate::parser::value::traits::$first_current<$var>>::is_right_assoc() {
                                    self.[<parse_ $first_current:lower _level>]()?
                                } else {
                                    self.[<parse_ $first_next:lower _level>]()?
                                };
                                if op == crate::common::ast::Op::Cons {
                                    lhs = crate::common::ast::Value::Cons(Box::new(lhs), Box::new(rhs));
                                }
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
        first_trait_overall = $first_trait_overall:ident,
        methods = { $( $methods:tt )* },
        all_traits = { $( $all_traits:ident, )* },
        chain = [ { $first_current:ident $(, $rest_current:ident)* } ]
    ) => {
        $(
            impl crate::parser::value::traits::$all_traits<$var> for $parser_struct {}
        )*
        impl crate::parser::value::traits::$first_current<$var> for $parser_struct {}
        $(
            impl crate::parser::value::traits::$rest_current<$var> for $parser_struct {}
        )*

        paste::paste! {
            impl $parser_struct {
                $( $methods )*

                fn [<parse_ $first_current:lower _level>] (&mut self) -> Result<crate::common::ast::value::Value<$var>, String> {
                    let mut lhs = self.parse_value_atom()?;
                    loop {
                        let maybe_op = {
                            let mut op = None;
                            if op.is_none() { op = <Self as crate::parser::value::traits::$first_current<$var>>::handle(self); }
                            $( if op.is_none() { op = <Self as crate::parser::value::traits::$rest_current<$var>>::handle(self); } )*
                            op
                        };
                        if let Some(op) = maybe_op {
                            let rhs = if <Self as crate::parser::value::traits::$first_current<$var>>::is_right_assoc() {
                                self.[<parse_ $first_current:lower _level>]()?
                            } else {
                                self.parse_value_atom()?
                            };
                             if op == crate::common::ast::core::Op::Cons {
                                lhs = crate::common::ast::value::Value::Cons(Box::new(lhs), Box::new(rhs));
                            }
                        } else { break; }
                    }
                    Ok(lhs)
                }
            }
        }

        impl crate::parser::value::traits::ValueParser<$var> for $parser_struct {
            fn parse_value(&mut self) -> Result<crate::common::ast::value::Value<$var>, String> {
                if let Some(token) = self.core().peek() {
                    $(
                        if <Self as crate::parser::value::traits::$dispatch_trait<$var>>::check(token) {
                            return <Self as crate::parser::value::traits::$dispatch_trait<$var>>::parse(self);
                        }
                    )*
                }
                paste::paste! {
                    self.[<parse_ $first_trait_overall:lower _level>]()
                }
            }

            fn parse_value_atom(&mut self) -> Result<crate::common::ast::value::Value<$var>, String> {
                if let Some(token) = self.core().peek().cloned() {
                    $(
                        if <Self as crate::parser::primitive::$primitive_trait<crate::common::ast::value::Value<$var>>>::check(&token) {
                            return <Self as crate::parser::primitive::$primitive_trait<crate::common::ast::value::Value<$var>>>::parse(self);
                        }
                    )*
                    $(
                        if <Self as crate::parser::value::traits::$dispatch_trait<$var>>::check(&token) {
                            return <Self as crate::parser::value::traits::$dispatch_trait<$var>>::parse(self);
                        }
                    )*
                }
                Err(format!("Unexpected token at atomic level: {:?}", self.core().peek()))
            }
        }
    };
}