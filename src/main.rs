mod token;
mod parser;
mod ast;
mod eval;

use token::tokenize;
use parser::Parser;
use eval::ml4::derive;

fn main() {
    let input = "|- let f = fun x -> match x with [] -> 0 | a :: b -> a in f (4::[]) + f [] + f (1 :: 2 :: 3 :: [])";

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
