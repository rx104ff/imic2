use std::collections::{HashSet};

use crate::common::ast::core::NamedVar;
use crate::common::ast::judgement::Judgment;
use crate::common::ast::r#type::{Scheme, Type};
use crate::parser::environment::traits::EnvironmentParser;
use crate::parser::primitive::states::TypeVarState;
use crate::{build_expression_parser, build_type_parser};
use crate::common::tokenizer::Token;
use crate::parser::primitive::{HasState, State};
use crate::parser::{ParserCore, TypeParser, ExpressionParser, BaseParser};


/// A recursive descent parser for the TypingML4 language.
pub struct Parser {
    core: ParserCore,
    states: std::collections::HashMap<std::any::TypeId, Box<dyn State>>,
}

build_expression_parser! {
    parser = Parser,
    var_type = NamedVar,

    primitive_parsers: [
        IntParsing,
        BoolParsing,
        NilParsing,
        GroupParsing,
        VariableParsing
    ],

    unary_parsers: [
        UnaryMinusParsing
    ],

    dispatch_parsers: [
        IfExprParsing,
        LetExprParsing,
        FunExprParsing,
        RecFunExprParsing,
        MatchExprParsing
    ],

    binop_chain: [
        { LtExprParsing },
        { ConsExprParsing },
        { AddExprParsing, SubExprParsing },
        { MulExprParsing },
        { AppExprParsing }
    ]
}


build_type_parser! {
    parser = Parser,
    var_type = NamedVar,

    primitive_parsers: [
        IntTypeParsing,
        BoolTypeParsing,
        GroupParsing
    ],
    postfix_parsers: [
        ListTypeParsing
    ],
    binop_chain: [
        { FunTypeParsing }
    ],
    stateful_primitives: [
        {
            base: TypeVarParsing,
            state_type: crate::parser::primitive::TypeVarState
        }
    ]
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            core: ParserCore::new(tokens),
            states: parser_type_impl::initialize_states(),
        }
    }

    /// The main entry point for the parser.
    /// It parses a judgment of the form `env |- expr : type`
    /// and returns the parsed Judgment struct.
    pub fn parse(&mut self) -> Result<(Judgment, HashSet<String>), String> {
        let env_with_schemes: Vec<(NamedVar, Scheme<NamedVar>)> =
            <Self as EnvironmentParser<NamedVar, Scheme<NamedVar>>>::parse_env_list(self)?;

        let env: Vec<(NamedVar, Type<NamedVar>)> = env_with_schemes
            .into_iter()
            .map(|(var, scheme)| (var, scheme.0))
            .collect();

        self.core.expect(Token::Turnstile)?;
        let expr = self.parse_expr()?;
        self.core.expect(Token::Colon)?;

        let ty = self.parse_type()?;

        let type_var_state: &TypeVarState = self.state();
        let used_names = type_var_state.get_used_names();

        Ok((Judgment::PolyInfer(env, expr, ty), used_names))
    }
}
