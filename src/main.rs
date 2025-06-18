mod token;
mod parser;
mod ast;
mod eval;
mod proof;

use token::tokenize;
use parser::Parser;
use eval::ml3::derive;

fn main() {
    let input = "f = ()[fun x -> match x with [] -> 0 | a :: b -> a] |- f (4::4::[]) + f([])";

    let tokens = tokenize(input);
    println!("{:?}", tokens);
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
