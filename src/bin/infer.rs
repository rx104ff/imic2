use std::env;
// The module is named `type`, so we use `r#type` to avoid conflict with the keyword.
use imic2::infer; 

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: cargo run --bin type_checker \"<JUDGMENT>\"");
        eprintln!("Example: cargo run --bin type_checker \"x:int |- if true then x else 0 : int\"");
        return;
    }

    let input = &args[1];

    // 1. Tokenize the input string.
    let tokens = infer::token::tokenize(input);
    
    // 2. The parser now consumes the entire judgment string (`env |- expr : type`)
    //    and returns a single, complete Judgment struct.
    let mut parser = infer::parser::Parser::new(tokens);
    let judgment = match parser.parse_judgment() {
        Ok(j) => j, // The parser now correctly returns the full Judgment.
        Err(e) => {
            eprintln!("Parsing Error: {}", e);
            return;
        }
    };

    // 3. Run the type checker on the parsed judgment.
    // The `judgment` object is now correctly instantiated and can be passed directly.
    match infer::infer::check_judgment(&judgment) {
        Ok(derivation) => {
            println!("{}", derivation);
        },
        Err(e) => eprintln!("Type Error: {}", e),
    }
}