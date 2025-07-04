use imic2::poly_infer;
use insta::assert_snapshot;

/// A helper function to simulate a full run of the polymorphic type inferrer.
/// It tokenizes, parses, and infers the type for a given judgment string.
fn run_poly_test(input: &str) -> String {
    poly_infer::ast::reset_type_var_counter();
    let tokens = poly_infer::token::tokenize(input);
    let mut parser = poly_infer::parser::Parser::new(tokens);
    let (judgment, used_names) = match parser.parse_judgment() {
        Ok(j) => j,
        Err(e) => return format!("Parsing Error: {}", e),
    };
    match poly_infer::poly_infer::infer_judgment(&judgment, used_names) {
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
    let result = run_poly_test("|- let id = fun x -> x in id id : bool -> bool");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_poly_typing_111() {
    let result = run_poly_test("f: 'a 'b.'a->'b->'a |- f 3 true + f 2 4 : int");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_poly_typing_112() {
    let result = run_poly_test("|- let k = fun x -> fun y -> x in (k 3 true) :: (k (1::[]) 3) : int list");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_poly_typing_113() {
    let result = run_poly_test("|- let compose = fun f -> fun g -> fun x -> f (g x) in
   let f = fun x -> if x then 3 else 4 in
   let g = fun x -> x < 4 in
   compose f (compose g f) true : int");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_poly_typing_114() {
    let result = run_poly_test("|- let twice = fun f -> fun x -> f (f x) in
   twice (fun x -> x + 4) 5 : int");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_poly_typing_115() {
    let result = run_poly_test("|- let twice = fun f -> fun x -> f (f x) in
   twice twice (fun x -> x + 4) 5 : int");
    assert_snapshot!(&result);
}

// TODO: This test produces inconsistant output
#[test]
fn test_infer_poly_typing_116() {
    let result = run_poly_test("|- let s = fun f -> fun g -> fun x -> f x (g x) in
   let k = fun x -> fun y -> x in
   s k k : 'a -> 'a");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_poly_typing_117() {
    let result = run_poly_test("|- let x = [] in let y = 3 :: x in true :: x : bool list");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_poly_typing_118() {
    
    let result = run_poly_test("|- let l = (fun x -> x) :: [] in
   let l1 = (fun y -> y + 1) :: l in
   (fun z -> if z then false else true) :: l : (bool -> bool) list");
    assert_snapshot!(&result);
}
