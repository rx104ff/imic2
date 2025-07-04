// src/main.rs
use std::env;
use std::io;

use imic2::common;
use imic2::eval::ast::LanguageVersion;
use imic2::eval::eval::derive;
use imic2::eval::parser::Parser;


fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 3 {
        eprintln!("Usage: {} <RULE> \"<PROGRAM>\"", args[0]);
        eprintln!("Available rules: ML1, ML2, ML3, ML4");
        return Ok(());
    }

    let rule = &args[1];
    let mut program = args[2].clone();

    let version = match rule.as_str() {
        "ML1" => LanguageVersion::ML1,
        "ML2" => LanguageVersion::ML2,
        "ML3" => LanguageVersion::ML3,
        "ML4" => LanguageVersion::ML4,
        _ => {
            eprintln!("Error: Unknown rule '{}'", rule);
            eprintln!("Available rules: ML1, ML2, ML3, ML4");
            return Ok(());
        }
    };
    
    // For ML1, which has no environment, prepend an empty one for the parser.
    if version == LanguageVersion::ML1 {
        program = format!("|- {}", program);
    }

    let tokens = common::token::tokenize(&program);
    let mut parser = Parser::new(tokens);
    let (env, expr) = parser.parse_program();

    let derivation = derive(&env, &expr, version);
    println!("{}", derivation);

    Ok(())
}
