use imic2::common;
use imic2::common::ast::NamelessValue;
use imic2::common::tokenizer::tokenize;
use imic2::nameless::parser::Parser;
use imic2::nameless::eval::{derive, derive_judgement};
use insta::assert_snapshot;

/// A helper function to simulate a full run of the nameless evaluator.
fn run_nameless_eval_test(input: &str) -> String {
       

    // 2. Parse the expression
    let tokens = common::tokenizer::tokenize(&input);
    print!("{:?}", tokens);
    let mut parser = Parser::new(tokens);
    let judgment = match parser.parse() {
        Ok(e) => e,
        Err(e) => return format!("Parsing Error: {}", e),
    };

    // 3. Run the derivation
    // match derive(&env, &expr) {
    //     Ok(derivation) => format!("{}", derivation),
    //     Err(e) => format!("Evaluation Error: {}", e),
    // }
    match derive_judgement(&judgment) {
        Ok(derivation) => format!("{}", derivation),
        Err(e) => format!("Evaluation Error: {}", e),
    }
}

#[test]
fn test_nameless_55() {
    let result = run_nameless_eval_test("true, 4 |- if #2 then #1 + 1 else #1 - 1 evalto 5");
    assert_snapshot!(&result);
}

#[test]
fn test_nameless_57() {
    let result = run_nameless_eval_test("|- let . = 3 * 3 in let . = 4 * #1 in #2 + #1 evalto 45");
    assert_snapshot!(&result);
}

#[test]
fn test_nameless_59() {
    let result = run_nameless_eval_test("3 |- let . = #1 * 2 in #1 + #1 evalto 12");
    assert_snapshot!(&result);
}

