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
    Equals, 
    Bar, 
    Minus, 
    Plus, 
    Star, 
    Lt, 
    Arrow, 
    Colon, 
    ColonColon, 
    Comma, 
    Turnstile,
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
pub fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut s: &str = input;

    while !s.is_empty() {
        // --- Multi-character symbols first ---
        if s.starts_with("|-") {
            tokens.push(Token::Turnstile);
            s = &s[2..];
        } else if s.starts_with("->") {
            tokens.push(Token::Arrow);
            s = &s[2..];
        } else if s.starts_with("::") {
            tokens.push(Token::ColonColon);
            s = &s[2..];
        } else if s.starts_with("[]") {
            tokens.push(Token::Nil);
            s = &s[2..];
        // --- Keywords and Identifiers ---
        } else if s.chars().next().map_or(false, |c| c.is_alphabetic()) {
            let end = s.find(|c: char| !c.is_alphanumeric()).unwrap_or(s.len());
            let keyword = &s[..end];
            s = &s[end..];
            match keyword {
                "let" => tokens.push(Token::Let),
                "in" => tokens.push(Token::In),
                "if" => tokens.push(Token::If),
                "then" => tokens.push(Token::Then),
                "else" => tokens.push(Token::Else),
                "match" => tokens.push(Token::Match),
                "with" => tokens.push(Token::With),
                "fun" => tokens.push(Token::Fun),
                "rec" => tokens.push(Token::Rec),
                "true" | "True" => tokens.push(Token::Bool(true)),
                "false" | "False" => tokens.push(Token::Bool(false)),
                _ => tokens.push(Token::Ident(keyword.to_string())),
            }
        // --- Numbers ---
        } else if s.chars().next().map_or(false, |c| c.is_ascii_digit()) {
            let end = s.find(|c: char| !c.is_ascii_digit()).unwrap_or(s.len());
            let num_str = &s[..end];
            s = &s[end..];
            tokens.push(Token::Int(num_str.parse().unwrap()));
        // --- Single-character symbols and whitespace ---
        } else {
            let c = s.chars().next().unwrap();
            match c {
                '\'' => { // Handle type variables like 'a
                    s = &s[1..]; // consume '
                    let end = s.find(|c: char| !c.is_alphanumeric()).unwrap_or(s.len());
                    let name = &s[..end];
                    s = &s[end..];
                    tokens.push(Token::TypeVar(name.to_string()));
                }
                '(' => tokens.push(Token::LParen),
                ')' => tokens.push(Token::RParen),
                '=' => tokens.push(Token::Equals),
                '|' => tokens.push(Token::Bar),
                '-' => tokens.push(Token::Minus),
                '+' => tokens.push(Token::Plus),
                '*' => tokens.push(Token::Star),
                '<' => tokens.push(Token::Lt),
                ':' => tokens.push(Token::Colon),
                ',' => tokens.push(Token::Comma),
                '.' => tokens.push(Token::Dot),
                c if c.is_whitespace() => { /* Skip */ }
                _ => panic!("Unexpected character: {}", c),
            };
            if !c.is_whitespace() && c != '\'' {
                 s = &s[1..];
            }
        }
    }
    tokens
}