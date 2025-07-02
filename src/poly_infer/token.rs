#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Keywords
    Let, In, If, Then, Else, Match, With, Fun, Rec,

    // Symbols
    Equals, Bar, Minus, Plus, Star, Lt, Arrow, Colon, ColonColon, Comma, Turnstile,
    Dot, // .

    // Literals and Identifiers
    Int(i64),
    Bool(bool),
    Ident(String),
    TypeVar(String), // 'a, 'b, etc.

    // Grouping and Lists
    LParen, RParen, Nil,
}

/// Tokenizes an input string into a sequence of Tokens for PolyTypingML4.
/// This version correctly handles all tokens and avoids infinite loops.
pub fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&c) = chars.peek() {
        match c {
            // Skip all whitespace characters.
            c if c.is_whitespace() => {
                chars.next(); // Consume and continue to the next character.
            }
            // Handle type variables like 'a or 'b
            '\'' => {
                chars.next(); // Consume '\''
                let mut name = String::new();
                while let Some(&next_c) = chars.peek() {
                    if next_c.is_alphanumeric() {
                        name.push(chars.next().unwrap());
                    } else {
                        break;
                    }
                }
                if name.is_empty() {
                    panic!("Invalid type variable: missing name after '");
                }
                tokens.push(Token::TypeVar(name));
            }
            // Handle keywords and identifiers
            c if c.is_alphabetic() => {
                let mut ident = String::new();
                while let Some(&next_c) = chars.peek() {
                    if next_c.is_alphanumeric() {
                        ident.push(chars.next().unwrap());
                    } else {
                        break;
                    }
                }
                let token = match ident.as_str() {
                    "let" => Token::Let, "in" => Token::In, "if" => Token::If,
                    "then" => Token::Then, "else" => Token::Else, "match" => Token::Match,
                    "with" => Token::With, "fun" => Token::Fun, "rec" => Token::Rec,
                    "true" | "True" => Token::Bool(true), "false" | "False" => Token::Bool(false),
                    _ => Token::Ident(ident),
                };
                tokens.push(token);
            }
            // Handle numbers
            c if c.is_ascii_digit() => {
                let mut number = String::new();
                while let Some(&next_c) = chars.peek() {
                    if next_c.is_ascii_digit() {
                        number.push(chars.next().unwrap());
                    } else {
                        break;
                    }
                }
                tokens.push(Token::Int(number.parse().unwrap()));
            }
            // Handle multi-character symbols
            '-' => {
                chars.next(); // Consume '-'
                if chars.peek() == Some(&'>') {
                    chars.next(); // Consume '>'
                    tokens.push(Token::Arrow);
                } else {
                    tokens.push(Token::Minus);
                }
            }
            ':' => {
                chars.next(); // Consume ':'
                if chars.peek() == Some(&':') {
                    chars.next(); // Consume second ':'
                    tokens.push(Token::ColonColon);
                } else {
                    tokens.push(Token::Colon);
                }
            }
            '|' => {
                chars.next(); // Consume '|'
                if chars.peek() == Some(&'-') {
                    chars.next(); // Consume '-'
                    tokens.push(Token::Turnstile);
                } else {
                    tokens.push(Token::Bar);
                }
            }
            '[' => {
                chars.next(); // Consume '['
                if chars.peek() == Some(&']') {
                    chars.next(); // Consume ']'
                    tokens.push(Token::Nil);
                } else {
                    panic!("Unmatched '['");
                }
            }
            // Handle single-character symbols
            '(' => { chars.next(); tokens.push(Token::LParen); }
            ')' => { chars.next(); tokens.push(Token::RParen); }
            '=' => { chars.next(); tokens.push(Token::Equals); }
            '+' => { chars.next(); tokens.push(Token::Plus); }
            '*' => { chars.next(); tokens.push(Token::Star); }
            '<' => { chars.next(); tokens.push(Token::Lt); }
            ',' => { chars.next(); tokens.push(Token::Comma); }
            '.' => { chars.next(); tokens.push(Token::Dot); }
            _ => {
                panic!("Unexpected character: {}", c);
            }
        }
    }
    tokens
}
