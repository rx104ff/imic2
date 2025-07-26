use std::collections::{HashMap, HashSet};

use crate::common::ast::core::NamedVar;
use crate::common::ast::expr::Expr;
use crate::common::ast::judgement::Judgment;
use crate::common::ast::r#type::{PolyTypeEnv, TyScheme, TypeVar};
use crate::{build_expression_parser, build_type_parser};
use crate::common::tokenizer::Token;
use crate::parser::primitive::traits::VariableParsing;
use crate::parser::{ParserCore, TypeParser, ExpressionParser, BaseParser};


/// A recursive descent parser for the TypingML4 language.
pub struct Parser {
    core: ParserCore,
    type_var_map: HashMap<String, TypeVar>,
    next_parser_var_id: usize,
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
    ]
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            core: ParserCore::new(tokens),
            type_var_map: HashMap::new(),
            next_parser_var_id: 0,
        }
    }

    /// The main entry point for the parser.
    /// It parses a judgment of the form `env |- expr : type`
    /// and returns the parsed Judgment struct.
    pub fn parse(&mut self) -> Result<(Judgment, HashSet<String>), String> {
        let env = self.parse_type_env()?;
        self.core.expect(Token::Turnstile)?;
        let expr = self.parse_expr()?;
        self.core.expect(Token::Colon)?;
        let ty = self.parse_type()?;
        let used_names = self.type_var_map.keys().map(|s| format!("'{}", s)).collect();
        Ok((Judgment::PolyInfer(env, expr, ty), used_names))
    }

    fn get_or_create_parser_var(&mut self, name: String) -> TypeVar {
        if let Some(var) = self.type_var_map.get(&name) {
            return var.clone();
        }
        let id = self.next_parser_var_id;
        self.next_parser_var_id += 1;
        let tv = TypeVar { id, name: format!("'{}", name) };
        self.type_var_map.insert(name, tv.clone());
        tv
    }

    // --- Type Environment and Type Parsing ---
    fn parse_type_env(&mut self) -> Result<PolyTypeEnv, String> {
        let mut env = PolyTypeEnv::new();
        if self.core.peek() == Some(&Token::Turnstile) { return Ok(env); }
        loop {
            let var = <Self as VariableParsing<Expr<NamedVar>>>::parse(self)?.into_variable().ok_or("Expected a variable name in `let` expression, but found something else.")?;
            self.core.expect(Token::Colon)?;
            let scheme = self.parse_type_scheme()?;
            env.push((var, scheme));
            if self.core.peek() == Some(&Token::Comma) { self.core.advance(); } 
            else { break; }
        }
        Ok(env)
    }

    // Parses a full type scheme, including `forall` quantifiers.
    fn parse_type_scheme(&mut self) -> Result<TyScheme<NamedVar>, String> {
        let mut quantified_vars = vec![];
        let mut potential_var_names = vec![];
        let initial_pos = self.core.pos();

        while let Some(Token::TypeVar(name)) = self.core.peek().cloned() {
            potential_var_names.push(name);
            self.core.advance();
        }

        if self.core.peek() == Some(&Token::Dot) {
            self.core.advance();
            potential_var_names.sort();
            for name in potential_var_names {
                let tv = self.get_or_create_parser_var(name);
                quantified_vars.push(tv);
            }
        } else {
            self.core.initial(initial_pos);
        }

        let ty = self.parse_type()?;
        Ok(TyScheme { vars: quantified_vars, ty })
    }    
}
