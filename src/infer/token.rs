#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Keywords
    Let,
    In,
    If,
    Then,
    Else,
    Match,
    With,
    Fun,
    Rec,

    // Symbols
    Equals,     // =
    Bar,        // |
    Minus,      // -
    Plus,       // +
    Star,       // *
    Lt,         // <
    Arrow,      // ->
    Colon,      // :      <-- NEW for type annotations
    ColonColon, // ::
    Comma,      // ,
    Turnstile,  // |-

    // Literals and Identifiers
    Int(i64),
    Bool(bool),
    Ident(String),

    // Grouping and Lists
    LParen,
    RParen,
    Nil,
}

/// Tokenizes an input string into a sequence of Tokens for TypingML4.
pub fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&ch) = chars.peek() {
        match ch {
            c if c.is_whitespace() => {
                chars.next(); // Consume whitespace
            }
            // Handle multi-character symbols first
            '|' => {
                chars.next();
                if chars.peek() == Some(&'-') {
                    chars.next();
                    tokens.push(Token::Turnstile);
                } else {
                    tokens.push(Token::Bar);
                }
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
            ':' => {
                chars.next();
                if chars.peek() == Some(&':') {
                    chars.next();
                    tokens.push(Token::ColonColon);
                } else {
                    tokens.push(Token::Colon); // Handles single colon
                }
            }
            '[' => {
                chars.next();
                if chars.peek() == Some(&']') {
                    chars.next();
                    tokens.push(Token::Nil);
                } else {
                    panic!("'[' must be followed by ']' for an empty list literal.");
                }
            }
            // Handle keywords and identifiers
            c if c.is_alphabetic() => {
                let mut ident = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_alphanumeric() || c == '_' {
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
                    "true" | "True" => Token::Bool(true),
                    "false" | "False" => Token::Bool(false),
                    // Type names like "int", "bool", "list" and variables are all tokenized as Ident.
                    // The parser will be responsible for interpreting them correctly based on context.
                    _ => Token::Ident(ident),
                };
                tokens.push(token);
            }
            // Handle numbers
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
            // Handle single-character symbols
            '=' => { chars.next(); tokens.push(Token::Equals); }
            '+' => { chars.next(); tokens.push(Token::Plus); }
            '*' => { chars.next(); tokens.push(Token::Star); }
            '<' => { chars.next(); tokens.push(Token::Lt); }
            '(' => { chars.next(); tokens.push(Token::LParen); }
            ')' => { chars.next(); tokens.push(Token::RParen); }
            ',' => { chars.next(); tokens.push(Token::Comma); }
            _ => {
                panic!("Unexpected character: {}", ch);
            }
        }
    }
    tokens
}
