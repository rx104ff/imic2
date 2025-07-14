use std::env;
use imic2::{common::{self}}; 

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: cargo run --bin nameless \"<JUDGMENT>\"");
        eprintln!("Example: cargo run --bin nameless \"x |- 3\"");
        return;
    }

    let input = &args[1];

    // 1. Tokenize the input string.
    let tokens = common::tokenizer::tokenize(input);

    // // 2. Parse the tokens into a Judgment struct.
    // let mut parser = poly_infer::parser::Parser::new(tokens);
    // let (judgment, used_names) = match parser.parse() {
    //     Ok(j) => j,
    //     Err(e) => {
    //         eprintln!("Parsing Error: {}", e);
    //         return;
    //     }
    // };

    // // 3. Run the type inferrer on the parsed judgment.
    // match poly_infer::poly_infer::infer_judgment(&judgment, used_names) {
    //     Ok(derivation) => {
    //         // The derivation object contains the full proof tree with all types resolved.
    //         // We can now print it directly.
    //         println!("{}", derivation);
    //     },
    //     Err(e) => eprintln!("Type Error: {}", e),
    // }
}
