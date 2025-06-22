#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Z,
    S,
    LParen,
    RParen,
    Plus,
    Times,
    PlusOp,
    TimesOp,
    Is,
    Less,
    Than,
    Evalto,
    Arrow,     // --->;
    ArrowD,    // -d->;
    ArrowStar, // -*->
}

pub fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            '(' => tokens.push(Token::LParen),
            ')' => tokens.push(Token::RParen),
            '+' => tokens.push(Token::PlusOp),
            '*' => tokens.push(Token::TimesOp),
            
            // --- NEW LOGIC TO HANDLE REDUCTION SYMBOLS ---
            '-' => {
                // This logic peeks ahead to see which arrow symbol it is.
                match chars.peek() {
                    Some('d') => {
                        chars.next(); // consume 'd'
                        if chars.next() == Some('-') && chars.next() == Some('>') {
                            tokens.push(Token::ArrowD);
                        } else { panic!("Invalid token starting with -d"); }
                    },
                    Some('*') => {
                        chars.next(); // consume '*'
                        if chars.next() == Some('-') && chars.next() == Some('>') {
                            tokens.push(Token::ArrowStar);
                        } else { panic!("Invalid token starting with -*"); }
                    },
                    Some('-') => {
                        chars.next(); // consume '-'
                        if chars.next() == Some('-') && chars.next() == Some('>') {
                            tokens.push(Token::Arrow);
                        } else { panic!("Invalid token starting with --"); }
                    },
                    _ => panic!("Unexpected character after '-'"),
                }
            }
            // --- End of new logic ---

            c if c.is_whitespace() => (),
            c if c.is_alphabetic() => {
                let mut keyword = String::new();
                keyword.push(c);
                while let Some(&next_c) = chars.peek() {
                    if next_c.is_alphabetic() {
                        keyword.push(chars.next().unwrap());
                    } else { break; }
                }
                match keyword.as_str() {
                    "Z" => tokens.push(Token::Z),
                    "S" => tokens.push(Token::S),
                    "is" => tokens.push(Token::Is),
                    "less" => tokens.push(Token::Less),
                    "than" => tokens.push(Token::Than),
                    "plus" => tokens.push(Token::Plus),
                    "times" => tokens.push(Token::Times),
                    "evalto" => tokens.push(Token::Evalto),
                    _ => panic!("Unknown keyword: {}", keyword),
                }
            }
            _ => panic!("Unexpected character: {}", c),
        }
    }
    tokens
}
