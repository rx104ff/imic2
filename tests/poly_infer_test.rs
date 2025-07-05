use imic2::{common, poly_infer};
use insta::assert_snapshot;

/// A helper function to simulate a full run of the polymorphic type inferrer.
/// It tokenizes, parses, and infers the type for a given judgment string.
fn run_poly_test(input: &str) -> String {
    let tokens = common::tokenizer::tokenize(input);
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

#[test]
fn test_infer_poly_typing_119() {
    let result = run_poly_test("|- let rec length = fun l ->    match l with [] -> 0 | x :: y -> 1 + length y in
    length (3 :: 2 :: []) + length ((1 :: []) :: []) : int");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_poly_typing_120() {
    let result = run_poly_test("|- let rec map = fun f -> fun l -> match l with [] -> [] | x :: y -> f x :: map f y in
    map (fun x -> x < 3) (map (fun x -> x * 2) (4 :: 5 :: 1 :: [])) : bool list");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_poly_typing_121() {
    let result = run_poly_test("|- let rec map = fun f -> fun l -> match l with [] -> [] | x :: y -> f x :: map f y in
    let f = map (fun x -> x) in
    let a = f (3 :: []) in f (true :: []) : bool list");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_poly_typing_122() {
    let result = run_poly_test("|- let f = fun x -> 
             let g = fun y -> x :: [] in 
             if true then g 3 else g false in
    match f 2 with [] -> f true | x :: y -> [] : bool list");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_poly_typing_123() {
    let result = run_poly_test("|- let f = fun x -> 
             let g = fun y -> y x :: [] in g (fun z -> 4) in
    match f true with [] -> 3 :: [] | x :: y -> f x : int list");
    assert_snapshot!(&result);
}
