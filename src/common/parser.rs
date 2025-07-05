use crate::common::tokenizer::Token;

pub trait Parsing {
    type Output;

    fn new(tokens: Vec<Token>) -> Self;
    
    fn parse(&mut self) -> Result<Self::Output, String>;
}

pub struct ParserCore {
    tokens: Vec<Token>,
    pos: usize,
}

impl ParserCore {
    pub fn new(tokens: Vec<Token>) -> Self {
        ParserCore { tokens, pos: 0 }
    }

    pub fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    pub fn advance(&mut self) {
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
    }

    pub fn expect(&mut self, expected: &Token) -> Result<(), String> {
        match self.peek() {
            Some(token) if token == expected => {
                self.advance();
                Ok(())
            }
            Some(token) => Err(format!("Expected token {:?}, found {:?}", expected, token)),
            None => Err(format!("Expected token {:?}, but found end of input.", expected)),
        }
    }
}
