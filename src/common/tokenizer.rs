#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // ML & PolyInfer Keywords
    Let, In, If, Then, Else, Match, With, Fun, Rec,
    
    // Nat Keywords
    Is, Less, Than, PlusKw, TimesKw, Z, S,
    
    // Symbols
    Equals, Bar, Minus, Plus, Star, Lt, Colon, ColonColon, Comma, Turnstile, Dot,
    
    // Literals and Identifiers
    Int(i64),
    Bool(bool),
    Ident(String),
    HashVar(u64),
    TypeVar(String), // 'a, 'b, etc.

    // Grouping and Lists
    LParen, RParen, LBracket, RBracket, Nil,
    
    // Evaluation and Reduction
    Evalto,
    Arrow,     // --->
    ArrowD,    // -d->
    ArrowStar, // -*->
    
    // End of File
    EOF,
}

/// A single, unified tokenizer for all language systems.
pub fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut s: &str = input;

    while !s.is_empty() {
        // --- Multi-character symbols first for unambiguous parsing ---
        if s.starts_with("-d->") {
            tokens.push(Token::ArrowD);
            s = &s[4..];
        } else if s.starts_with("-*->") {
            tokens.push(Token::ArrowStar);
            s = &s[4..];
        } else if s.starts_with("--->") {
            // Note: This could conflict with ML's `->` if not handled carefully.
            // We'll assume the parser for each language knows which arrow it expects.
            tokens.push(Token::Arrow);
            s = &s[4..];
        } else if s.starts_with("->") {
            tokens.push(Token::Arrow);
            s = &s[2..];
        } else if s.starts_with("::") {
            tokens.push(Token::ColonColon);
            s = &s[2..];
        } else if s.starts_with("[]") {
            tokens.push(Token::Nil);
            s = &s[2..];
        } else if s.starts_with("|-") {
            tokens.push(Token::Turnstile);
            s = &s[2..];
        // --- Keywords and Identifiers ---
        } else if s.chars().next().map_or(false, |c| c.is_alphabetic()) {
            let end = s.find(|c: char| !c.is_alphanumeric()).unwrap_or(s.len());
            let keyword = &s[..end];
            s = &s[end..];
            match keyword {
                // Nat Keywords
                "Z" => tokens.push(Token::Z),
                "S" => tokens.push(Token::S),
                "is" => tokens.push(Token::Is),
                "less" => tokens.push(Token::Less),
                "than" => tokens.push(Token::Than),
                "plus" => tokens.push(Token::PlusKw),
                "times" => tokens.push(Token::TimesKw),

                // ML Keywords
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
                "evalto" => tokens.push(Token::Evalto),
                _ => tokens.push(Token::Ident(keyword.to_string())),
            }
        // --- Numbers ---
        } else if s.chars().next().map_or(false, |c| c.is_ascii_digit()) {
            let end = s.find(|c: char| !c.is_ascii_digit()).unwrap_or(s.len());
            let num_str = &s[..end];
            s = &s[end..];
            tokens.push(Token::Int(num_str.parse().unwrap()));
        // --- Single-character symbols ---
        } else {
            let c = s.chars().next().unwrap();
            match c {
                '#' => {
                    s = &s[1..];
                    let end = s.find(|c: char| !c.is_ascii_digit()).unwrap_or(s.len());
                    let num_str = &s[..end];
                    s = &s[end..];
                    if let Ok(num) = num_str.parse() {
                        tokens.push(Token::HashVar(num));
                    } else {
                        panic!("Expect a number")
                    }
                }
                '\'' => {
                    s = &s[1..];
                    let end = s.find(|c: char| !c.is_alphanumeric()).unwrap_or(s.len());
                    let name = &s[..end];
                    s = &s[end..];
                    tokens.push(Token::TypeVar(name.to_string()));
                }
                '(' => { tokens.push(Token::LParen); s = &s[1..]; }
                ')' => { tokens.push(Token::RParen); s = &s[1..]; }
                '[' => { tokens.push(Token::LBracket); s = &s[1..]; }
                ']' => { tokens.push(Token::RBracket); s = &s[1..]; }
                '=' => { tokens.push(Token::Equals); s = &s[1..]; }
                '|' => { tokens.push(Token::Bar); s = &s[1..]; }
                '-' => { tokens.push(Token::Minus); s = &s[1..]; }
                '+' => { tokens.push(Token::Plus); s = &s[1..]; }
                '*' => { tokens.push(Token::Star); s = &s[1..]; }
                '<' => { tokens.push(Token::Lt); s = &s[1..]; }
                ':' => { tokens.push(Token::Colon); s = &s[1..]; }
                ',' => { tokens.push(Token::Comma); s = &s[1..]; }
                '.' => { tokens.push(Token::Dot); s = &s[1..]; }
                c if c.is_whitespace() => { s = &s[1..]; }
                _ => panic!("Unexpected character: {}", c),
            };
        }
    }
    tokens
}