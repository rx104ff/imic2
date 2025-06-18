// src/token.rs
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Let,
    In,
    If,
    Then,
    Else,
    Match,
    With,
    Fun,
    Rec,
    Lambda,
    Arrow,
    Equals,
    Bar,
    Minus,
    Plus,
    Star,
    Lt,
    ColonColon,
    Int(i64),
    Bool(bool),
    Ident(String),
    LParen,
    RParen,
    Nil,  
    LBracket,
    RBracket,
    Comma,     // <-- Added here
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
            c if c.is_ascii_alphabetic() => {
                let mut ident = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_alphanumeric() {
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
                    "match" => Token::Match,
                    "with" => Token::With,
                    "fun" => Token::Fun,
                    "rec" => Token::Rec,
                    "true" => Token::Bool(true),
                    "false" => Token::Bool(false),
                    _ => Token::Ident(ident),
                };
                tokens.push(token);
            }
            c if c.is_ascii_digit() => {
                let mut number = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_digit() {
                        number.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                tokens.push(Token::Int(number.parse().unwrap()));
            }
            '=' => {
                chars.next();
                tokens.push(Token::Equals);
            }
            '|' => {
                chars.next();
                tokens.push(Token::Bar);
            }
            '-' => {
                chars.next();
                if chars.peek() == Some(&'>') {
                    chars.next();
                    tokens.push(Token::Arrow);
                } else {
                    tokens.push(Token::Minus);
                }
            }
            '+' => {
                chars.next();
                tokens.push(Token::Plus);
            }
            '*' => {
                chars.next();
                tokens.push(Token::Star);
            }
            '<' => {
                chars.next();
                tokens.push(Token::Lt);
            }
            ':' => {
                chars.next();
                if chars.peek() == Some(&':') {
                    chars.next();
                    tokens.push(Token::ColonColon);
                } else {
                    panic!("Unexpected character after ':'");
                }
            }
            '(' => {
                chars.next();
                tokens.push(Token::LParen);
            }
            ')' => {
                chars.next();
                tokens.push(Token::RParen);
            }
            '[' => {
                chars.next(); // consume '['
                if let Some(&']') = chars.peek() {
                    chars.next(); // consume ']'
                    tokens.push(Token::Nil);
                } else {
                    tokens.push(Token::LBracket);
                }
            }
            ']' => {
                chars.next();
                tokens.push(Token::RBracket);
            }
            ',' => {
                chars.next();
                tokens.push(Token::Comma); // <-- Added here
            }
            _ => {
                panic!("Unexpected character: {}", ch);
            }
        }
    }

    tokens.push(Token::EOF);
    tokens
}
