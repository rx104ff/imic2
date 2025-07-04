use crate::nat::ast::{Nat, Expr, Judgment, ArithmeticOp, ReductionType};
use crate::common::token::Token;

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
            None => Err(format!("Expected token {:?}, but found the end of input.", expected)),
        }
    }

    fn parse_nat(&mut self) -> Result<Nat, String> {
        match self.peek() {
            Some(Token::Z) => {
                self.advance();
                Ok(Nat::Z)
            }
            Some(Token::S) => {
                self.advance();
                self.expect(Token::LParen)?;
                let inner = self.parse_nat()?;
                self.expect(Token::RParen)?;
                Ok(Nat::S(Box::new(inner)))
            }
            _ => Err("Expected 'Z' or 'S' to start a Nat expression".to_string()),
        }
    }

    fn parse_factor(&mut self) -> Result<Expr, String> {
        match self.peek() {
            Some(Token::LParen) => {
                self.advance();
                let exp = self.parse_expr()?; // Start a new expression inside parens
                self.expect(Token::RParen)?;
                Ok(exp)
            }
            Some(Token::Z) | Some(Token::S) => Ok(Expr::N(self.parse_nat()?)),
            _ => Err("Expected a Nat value or a parenthesized expression".to_string()),
        }
    }

    fn parse_term(&mut self) -> Result<Expr, String> {
        let mut lhs = self.parse_factor()?;
        while let Some(Token::Star) = self.peek() {
            self.advance(); // Consume '*'
            let rhs = self.parse_factor()?;
            lhs = Expr::Times(Box::new(lhs), Box::new(rhs));
        }
        Ok(lhs)
    }

    pub fn parse_expr(&mut self) -> Result<Expr, String> {
        let mut lhs = self.parse_term()?;
        while let Some(Token::Plus) = self.peek() {
            self.advance(); // Consume '+'
            let rhs = self.parse_term()?;
            lhs = Expr::Plus(Box::new(lhs), Box::new(rhs));
        }
        Ok(lhs)
    }

    /// The main parser entry point, updated to call the new top-level `parse_expr`.
    pub fn parse_judgment(&mut self) -> Result<Judgment, String> {
        if self.peek().is_none() {
            return Err("Cannot parse an empty input.".to_string());
        }

        // The entry point for parsing is always to parse the left-hand side expression first.
        let lhs_expr = self.parse_expr()?;

        // Now, we look at the next token to determine the type of judgment.
        match self.peek() {
            // Case 1: Evaluation `e evalto n`
            Some(Token::Evalto) => {
                self.advance();
                let n = self.parse_nat()?;
                Ok(Judgment::Evaluation { exp: lhs_expr, n })
            }
            // Case 2: Reduction `e1 ---> e2`
            Some(Token::Arrow) | Some(Token::ArrowD) | Some(Token::ArrowStar) => {
                let r_type = match self.peek().unwrap() {
                    Token::Arrow => ReductionType::Single,
                    Token::ArrowD => ReductionType::Direct,
                    Token::ArrowStar => ReductionType::Multi,
                    _ => unreachable!(),
                };
                self.advance();
                // The right-hand side of a reduction is also a full expression.
                let e2 = self.parse_expr()?;
                Ok(Judgment::Reduction { r_type, e1: lhs_expr, e2 })
            }
            // Case 3: Must be an Arithmetic or Comparison judgment.
            // For these, the left-hand side must have been a single Nat value.
            _ => {
                let n1 = match lhs_expr {
                    Expr::N(n) => n,
                    _ => return Err("Expected a single Nat value on the left side for 'plus', 'times', or 'is less than' judgments.".to_string()),
                };

                match self.peek() {
                    Some(Token::PlusKw) | Some(Token::TimesKw) => {
                        let op = if *self.peek().unwrap() == Token::PlusKw { ArithmeticOp::Plus } else { ArithmeticOp::Times };
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
                    // Add complete error handling for this inner match
                    None => Err("Incomplete judgment. Expected 'plus', 'times', or 'is' after Nat value.".to_string()),
                    Some(token) => Err(format!("Unexpected token '{:?}' following a Nat value.", token)),
                }
            }
        }
    }
}
