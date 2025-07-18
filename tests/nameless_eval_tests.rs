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

#[test]
fn test_nameless_61() {
    let result = run_nameless_eval_test("|- let . = let . = 3 - 2 in #1 * #1 in let . = 4 in #2 + #1 evalto 5");
    assert_snapshot!(&result);
}

#[test]
fn test_nameless_63() {
    let result = run_nameless_eval_test("|- let . = 2 in fun . -> #1 + #2 evalto (2)[fun . -> #1 + #2]");
    assert_snapshot!(&result);
}

#[test]
fn test_nameless_65() {
    let result = run_nameless_eval_test("|- let . = fun . -> #1 3 + #1 4 in #1 (fun . -> #1 * #1) evalto 25");
    assert_snapshot!(&result);
}

#[test]
fn test_nameless_67() {
    let result = run_nameless_eval_test("|- let . = 3 in let . = fun . -> #1 * #2 in let . = 5 in #2 4 evalto 12");
    assert_snapshot!(&result);
}

#[test]
fn test_nameless_69() {
    let result = run_nameless_eval_test("|- let rec . = fun . -> 
     if #1 < 2 then 1 else #1 * #2 (#1 - 1) in #1 3
   evalto 6");
    assert_snapshot!(&result);
}
