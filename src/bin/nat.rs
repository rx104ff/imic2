use std::env;
use imic2::{common, nat};

fn main() {
    let args: Vec<String> = env::args().collect();
    let (mode, input_str) = match args.len() {
        // Case 1: No mode provided (e.g., `cargo run --bin nat "judgment"`)
        2 => (None, &args[1]),
        // Case 2: Mode provided (e.g., `cargo run --bin nat CompareNat1 "judgment"`)
        3 => (Some(&args[1]), &args[2]),
        _ => {
            eprintln!("Usage: cargo run --bin nat [<MODE>] \"<JUDGMENT>\"");
            eprintln!("Default mode handles 'plus' and 'times'.");
            eprintln!("Optional modes: CompareNat1, CompareNat2, CompareNat3");
            return;
        }
    };

    // Parse the optional mode string into our enum
    let comparison_mode: Option<nat::ast::ComparisonMode> = match mode {
        None => None,
        Some(s) => match s.as_str() {
            "CompareNat1" => Some(nat::ast::ComparisonMode::V1),
            "CompareNat2" => Some(nat::ast::ComparisonMode::V2),
            "CompareNat3" => Some(nat::ast::ComparisonMode::V3),
            _ => {
                eprintln!("Error: Unknown comparison mode '{}'", s);
                return;
            }
        },
    };

    let tokens = common::token::tokenize(input_str);
    let mut parser = nat::parser::Parser::new(tokens);
    let judgment = match parser.parse_judgment() {
        Ok(j) => j,
        Err(e) => { eprintln!("Parsing Error: {}", e); return; }
    };

    // Call the derivator with the optional mode
    match nat::nat::derive(&judgment, comparison_mode) {
        Ok(derivation) => println!("{}", derivation),
        Err(e) => eprintln!("Derivation Error: {}", e),
    }
}
