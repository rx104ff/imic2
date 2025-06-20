use crate::nat::ast::{Nat, Judgment, ArithmeticOp};
use crate::nat::token::Token;

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn advance(&mut self) {
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
    }

    fn expect(&mut self, expected: Token) -> Result<(), String> {
        match self.peek() {
            Some(token) if *token == expected => {
                self.advance();
                Ok(())
            }
            Some(token) => Err(format!("Expected token {:?}, found {:?}", expected, token)),
            None => Err(format!("Expected token {:?}, found end of input", expected)),
        }
    }

    fn parse_nat(&mut self) -> Result<Nat, String> {
        match self.peek() {
            Some(Token::Z) => {
                self.advance();
                Ok(Nat::Z)
            }
            Some(Token::S) => {
                self.advance(); // Consume 'S'
                self.expect(Token::LParen)?;
                let inner_nat = self.parse_nat()?;
                self.expect(Token::RParen)?;
                Ok(Nat::S(Box::new(inner_nat)))
            }
            Some(token) => Err(format!("Unexpected token while parsing Nat: {:?}", token)),
            None => Err("Unexpected end of input while parsing Nat".to_string()),
        }
    }

    pub fn parse_judgment(&mut self) -> Result<Judgment, String> {
        let n1 = self.parse_nat()?;
        
        match self.peek() {
            Some(Token::Plus) | Some(Token::Times) => {
                let op = if *self.peek().unwrap() == Token::Plus { ArithmeticOp::Plus } else { ArithmeticOp::Times };
                self.advance();
                let n2 = self.parse_nat()?;
                self.expect(Token::Is)?;
                let n3 = self.parse_nat()?;
                Ok(Judgment::Arithmetic { op, n1, n2, n3 })
            }
            Some(Token::Is) => {
                self.advance();
                self.expect(Token::Less)?;
                self.expect(Token::Than)?;
                let n2 = self.parse_nat()?;
                Ok(Judgment::Comparison { n1, n2 })
            }
            _ => Err("Expected 'plus', 'times', or 'is' after first Nat".to_string()),
        }
    }
}