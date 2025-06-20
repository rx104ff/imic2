#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Z,
    S,
    LParen,
    RParen,
    Plus,
    Times,
    Is,
    Less,
    Than,
}

pub fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(c) = chars.next() {
        match c {
            '(' => tokens.push(Token::LParen),
            ')' => tokens.push(Token::RParen),
            c if c.is_whitespace() => (), // Skip whitespace
            c if c.is_alphabetic() => {
                let mut keyword = String::new();
                keyword.push(c);
                while let Some(&next_c) = chars.peek() {
                    if next_c.is_alphabetic() {
                        keyword.push(chars.next().unwrap());
                    } else {
                        break;
                    }
                }
                match keyword.as_str() {
                    "Z" => tokens.push(Token::Z),
                    "S" => tokens.push(Token::S),
                    "plus" => tokens.push(Token::Plus),
                    "times" => tokens.push(Token::Times),
                    "is" => tokens.push(Token::Is),
                    "less" => tokens.push(Token::Less),
                    "than" => tokens.push(Token::Than),
                    _ => panic!("Unknown keyword: {}", keyword),
                }
            }
            _ => panic!("Unexpected character: {}", c),
        }
    }
    tokens
}