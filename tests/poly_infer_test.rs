use imic2::poly_infer;
use insta::assert_snapshot;

/// A helper function to simulate a full run of the polymorphic type inferrer.
/// It tokenizes, parses, and infers the type for a given judgment string.
fn run_poly_test(input: &str) -> String {
    poly_infer::ast::reset_type_var_counter();
    let tokens = poly_infer::token::tokenize(input);
    let mut parser = poly_infer::parser::Parser::new(tokens);
    let judgment = match parser.parse_judgment() {
        Ok(j) => j,
        Err(e) => return format!("Parsing Error: {}", e),
    };
    match poly_infer::poly_infer::infer_judgment(&judgment) {
        Ok(derivation) => {
            // The derivation object contains the full proof tree with all types resolved.
            // We can now print it directly.
            format!("{}", derivation)
        }
        Err(e) => format!("Type Error: {}", e),
    }
}

#[test]
fn test_infer_poly_typing_107() {
    let result = run_poly_test("|- fun x -> x : 'a -> 'a");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_poly_typing_108() {
    let result = run_poly_test("f: 'a.'a->'a |- f 3 : int");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_poly_typing_109() {
    let result = run_poly_test("f: 'a.'a->'a |- f (fun x -> x + 3) : int -> int");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_poly_typing_110() {
    poly_infer::ast::reset_type_var_counter();
    let result = run_poly_test("|- let id = fun x -> x in id id : bool -> bool");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_poly_typing_111() {
    let result = run_poly_test("f: 'a 'b.'a->'b->'a |- f 3 true + f 2 4 : int");
    assert_snapshot!(&result);
}
