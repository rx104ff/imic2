use crate::{build_parser, common::{ast::{Judgment, NamelessExpr, NamelessVar}, parser::{BaseParser, ExpressionParser, ParserCore, ValueParser, VariableParser}, tokenizer::Token}};

pub struct Parser {
    core: ParserCore,
}

build_parser! {
    parser = Parser,
    var_type = NamelessVar,

    primitive_parsers: [
        IntParsing,
        BoolParsing,
        NilParsing,
        GroupParsing,
        VariableParser
    ],

    unary_parsers: [
        UnaryMinusParser
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

impl ValueParser for Parser {
    fn parse_inner_expr(&self, tokens: Vec<Token>) -> Result<NamelessExpr, String>{
        let mut inner_parser = Self::new(tokens);
        inner_parser.parse_expr()
    }
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { core: ParserCore::new(tokens) }
    }
    
    /// The unique entry point for the `eval` parser.
    /// It parses a judgment of the form `Γ ⊢ e evalto v`.
    pub fn parse(&mut self) -> Result<Judgment, String> {
        let env = <Self as VariableParser>::parse_env_list(self)?;
        <Self as BaseParser>::core(self).expect(Token::Turnstile)?;
        let expr = self.parse_expr()?;
        if let Some(Token::Evalto) = <Self as BaseParser>::core(self).peek() {
            while <Self as BaseParser>::core(self).peek().is_some() && <Self as BaseParser>::core(self).peek() != Some(&Token::EOF) {
                <Self as BaseParser>::core(self).advance();
            }
        }
        Ok(Judgment::NamelessEvaluation(env, expr))
    }
}
