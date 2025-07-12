use crate::common::{ast::{Judgment, NamelessExpr}, parser::{ExpressionParser, HasParseMode, NamelessMode, ParseMode, ParserCore, ValueParser}, tokenizer::Token};

pub struct Parser {
    core: ParserCore,
}

impl HasParseMode for Parser {
    type Mode = NamelessMode;
}

impl ExpressionParser<
    <<Parser as HasParseMode>::Mode as ParseMode>::Variable
> for Parser {
    fn core(&mut self) -> &mut ParserCore {
        &mut self.core
    }
}

impl ValueParser<NamelessExpr> for Parser {
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
        let env = if let Some(Token::Ident(_)) = self.core().peek() {
            <<Self as HasParseMode>::Mode as ParseMode>::parse_env_list(self)
        } else {
            Ok(vec![])
        }?;
        self.core().expect(Token::Turnstile)?;
        let expr = self.parse_expr()?;
        if let Some(Token::Evalto) = self.core().peek() {
            while self.core().peek().is_some() && self.core().peek() != Some(&Token::EOF) {
                self.core().advance();
            }
        }
        Ok(Judgment::NamelessEvaluation(env, expr))
    }
}
