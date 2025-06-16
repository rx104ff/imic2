// src/token.rs
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Let,
    In,
    If,
    Then,
    Else,
    Ident(String),
    Number(i64),
    Equals,
    Arrow,
    Lambda,
    LParen,
    RParen,
    Op(String),
    EOF,
}

pub fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&ch) = chars.peek() {
        match ch {
            c if c.is_whitespace() => {
                chars.next();
            }
            c if c.is_alphabetic() => {
                let mut ident = String::new();
                while let Some(c) = chars.peek().copied() {
                    if c.is_alphanumeric() {
                        ident.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                let token = match ident.as_str() {
                    "let" => Token::Let,
                    "in" => Token::In,
                    "if" => Token::If,
                    "then" => Token::Then,
                    "else" => Token::Else,
                    _ => Token::Ident(ident),
                };
                tokens.push(token);
            }
            c if c.is_digit(10) => {
                let mut num = String::new();
                while let Some(c) = chars.peek().copied() {
                    if c.is_digit(10) {
                        num.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                tokens.push(Token::Number(num.parse().unwrap()));
            }
            '=' => {
                chars.next();
                tokens.push(Token::Equals);
            }
            '-' => {
                chars.next();
                if chars.peek() == Some(&'>') {
                    chars.next();
                    tokens.push(Token::Arrow);
                } else {
                    tokens.push(Token::Op("-".to_string()));
                }
            }
            '\\' => {
                chars.next();
                tokens.push(Token::Lambda);
            }
            '(' => {
                chars.next();
                tokens.push(Token::LParen);
            }
            ')' => {
                chars.next();
                tokens.push(Token::RParen);
            }
            '+' | '*' | '/' => {
                let op = chars.next().unwrap();
                tokens.push(Token::Op(op.to_string()));
            }
            _ => {
                panic!("Unexpected character: {}", ch);
            }
        }
    }

    tokens.push(Token::EOF);
    tokens
}

