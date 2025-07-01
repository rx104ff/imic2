use imic2::infer;
use insta::assert_snapshot;

/// A helper function to simulate a full run of the type checker.
fn run_type_test(input: &str) -> String {
    let tokens = infer::token::tokenize(input);
    let mut parser = infer::parser::Parser::new(tokens);
    let judgment = match parser.parse_judgment() {
        Ok(j) => j,
        Err(e) => return format!("Parsing Error: {}", e),
    };

    match infer::infer::infer_judgment(&judgment) {
        Ok(derivation) => format!("{}", derivation),
        Err(e) => format!("Type Error: {}", e),
    }
}

#[test]
fn test_infer_typing_80() {
    let result = run_type_test("|- 3 + 5 : int");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_typing_81() {
    let result = run_type_test("|- if 4 < 5 then 2 + 3 else 8 * 8 : int");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_typing_82() {
    let result = run_type_test("x : bool, y : int |- if x then y + 1 else y - 1 : int");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_typing_83() {
    let result = run_type_test("|- let x = 3 < 2 in let y = 5 in if x then y else 2 : int");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_typing_84() {
    let result = run_type_test("|- fun x -> x + 1 : int -> int");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_typing_85() {
    let result = run_type_test("|- let f = fun x -> x + 1 in f 4 : int");
    assert_snapshot!(&result);
}
