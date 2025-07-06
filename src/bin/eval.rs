// src/main.rs
use std::env;

use imic2::common;
use imic2::eval::version::LanguageVersion;
use imic2::eval::eval::derive_judgement;
use imic2::eval::parser::Parser;


fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 3 {
        eprintln!("Usage: {} <RULE> \"<PROGRAM>\"", args[0]);
        eprintln!("Available rules: ML1, ML2, ML3, ML4");
        return;
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
            return;
        }
    };
    
    // For ML1, which has no environment, prepend an empty one for the parser.
    if version == LanguageVersion::ML1 {
        program = format!("|- {}", program);
    }

    let tokens = common::tokenizer::tokenize(&program);
    let mut parser = Parser::new(tokens);
    let judgment = match parser.parse_program() {
        Ok(j) => j, // The parser now correctly returns the full Judgment.
        Err(e) => {
            eprintln!("Parsing Error: {}", e);
            return;
        }
    };

    match derive_judgement(&judgment, version) {
        Ok(derivation) => {
            println!("{}", derivation);
        },
        Err(e) => eprintln!("Type Error: {}", e),
    };
}
