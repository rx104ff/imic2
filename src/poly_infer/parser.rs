use std::collections::{HashMap, HashSet};

use crate::common::ast::{Type, PolyTypeEnv, TyScheme, TypeVar, Judgment};
use crate::common::parser::{ExpressionParser, ParserCore, TypeParser};
use crate::common::tokenizer::Token;


/// A recursive descent parser for the TypingML4 language.
pub struct Parser {
    core: ParserCore,
    type_var_map: HashMap<String, TypeVar>,
    next_parser_var_id: usize,
}

impl ExpressionParser for Parser {
    fn core(&mut self) -> &mut ParserCore {
        &mut self.core
    }
}

impl TypeParser for Parser {
    fn core(&mut self) -> &mut ParserCore {
        &mut self.core
    }

    fn parse_single_type(&mut self) -> Result<Type, String> {
        let ty = match self.core.peek().cloned() {
            Some(Token::Ident(name)) => {
                self.core.advance();
                match name.as_str() {
                    "int" => Type::Int,
                    "bool" => Type::Bool,
                    _ => return Err(format!("Unknown type name '{}'", name)),
                }
            }
            Some(Token::TypeVar(name)) => {
                self.core.advance();
                let tv = self.get_or_create_parser_var(name);
                Type::Var(tv)
            }
            Some(Token::LParen) => {
                self.core.advance();
                let inner_ty = self.parse_type()?;
                self.core.expect(Token::RParen)?;
                inner_ty
            }
            _ => return Err("Expected a type name or a parenthesized type.".to_string()),
        };
        if let Some(Token::Ident(s)) = self.core.peek() {
            if s == "list" {
                self.core.advance();
                return Ok(Type::List(Box::new(ty)));
            }
        }
        Ok(ty)
    }
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
            let var = self.core.next_var()?;
            self.core.expect(Token::Colon)?;
            let scheme = self.parse_type_scheme()?;
            env.push((var, scheme));
            if self.core.peek() == Some(&Token::Comma) { self.core.advance(); } 
            else { break; }
        }
        Ok(env)
    }

    // Parses a full type scheme, including `forall` quantifiers.
    fn parse_type_scheme(&mut self) -> Result<TyScheme, String> {
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
