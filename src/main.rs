mod token;
mod parser;
mod ast;
mod eval;

use token::tokenize;
use parser::Parser;
use eval::ml4::derive;

fn main() {
    let input = "f = ()[fun x -> x + 1], y = 2 |- f y";

    let tokens = tokenize(input);
    let mut parser = Parser::new(tokens);
    let (env, expr) = parser.parse_program();

    println!("Parsed environment:");
    for (var, val) in env.iter() {
        println!("  {:?} = {:?}", var, val);
    }

    println!("\nParsed expression:\n  {:?}", expr);

    let derivation = derive(&env, &expr);
    println!("\nDerivation:\n{}", derivation);
}
