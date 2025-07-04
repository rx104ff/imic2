use imic2::{common, infer};
use insta::assert_snapshot;

/// A helper function to simulate a full run of the type checker.
fn run_type_test(input: &str) -> String {
    let tokens = common::token::tokenize(input);
    let mut parser = infer::parser::Parser::new(tokens);
    let judgment = match parser.parse_judgment() {
        Ok(j) => j,
        Err(e) => return format!("Parsing Error: {}", e),
    };

    match infer::infer::check_judgment(&judgment) {
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

#[test]
fn test_infer_typing_86() {
    let result = run_type_test("|- fun f -> f 0 + f 1 : (int -> int) -> int");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_typing_87() {
    let result = run_type_test("|- let max = fun x -> fun y -> if x < y then y else x in max 3 5 : int");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_typing_88() {
    let result = run_type_test("|- 4 :: [] : int list");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_typing_89() {
    let result = run_type_test("|- true :: false :: [] : bool list");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_typing_90() {
    let result = run_type_test("|- fun x -> fun y -> x : int -> int -> int");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_typing_91() {
    let result = run_type_test("|- fun x -> fun y -> x : bool -> int -> bool");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_typing_92() {
    let result = run_type_test("|- let k = fun x -> fun y -> x in k 3 true : int");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_typing_93() {
    let result = run_type_test("|- let k = fun x -> fun y -> x in k (1::[]) 3 : int list");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_typing_94() {
    let result = run_type_test("|- let k = fun x -> fun y -> x in k true (fun x -> x + 1) : bool");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_typing_95() {
    let result = run_type_test("|- let compose = fun f -> fun g -> fun x -> f (g x) in
   let p = fun x -> x * x in
   let q = fun x -> x + 4 in
   compose p q : int -> int");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_typing_96() {
    let result = run_type_test("|- let compose = fun f -> fun g -> fun x -> f (g x) in
   let p = fun x -> if x then 3 else 4 in
   let q = fun x -> x < 4 in
   compose p q : int -> int");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_typing_97() {
    let result = run_type_test("|- let s = fun f -> fun g -> fun x -> f x (g x) in
   let k1 = fun x -> fun y -> x in
   let k2 = fun x -> fun y -> x in
   s k1 k2 : int -> int");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_typing_98() {
    let result = run_type_test("|- let s = fun f -> fun g -> fun x -> f x (g x) in
   let k1 = fun x -> fun y -> x in
   let k2 = fun x -> fun y -> x in
   s k1 k2 (fun x -> x + 1) : int -> int");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_typing_99() {
    let result = run_type_test("|- let rec fact = fun n ->
     if n < 2 then 1 else n * fact (n - 1) in
     fact 3 : int");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_typing_100() {
    let result = run_type_test("|- let rec sum = fun f -> fun n ->
     if n < 1 then 0 else f n + sum f (n - 1) in 
   sum (fun x -> x * x) 2 : int");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_typing_101() {
    let result = run_type_test("|- let l = (fun x -> x) :: (fun y -> 2) :: (fun z -> z + 3) :: [] in 2 : int");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_typing_102() {
    let result = run_type_test("|- let rec length = fun l -> match l with [] -> 0 | x :: y -> 1 + length y in
    length : int list -> int");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_typing_103() {
    let result = run_type_test("|- let rec length = fun l -> match l with [] -> 0 | x :: y -> 1 + length y in
    length ((fun x -> x) :: (fun y -> y + 3) :: []) : int");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_typing_104() {
    let result = run_type_test("|- let rec append = fun l1 -> fun l2 -> 
     match l1 with [] -> l2 | x :: y -> x :: append y l2 in
     append : int list -> int list -> int list");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_typing_105() {
    let result = run_type_test("|- let rec append = fun l1 -> fun l2 -> 
     match l1 with [] -> l2 | x :: y -> x :: append y l2 in
     append (true :: []) (false :: []) : bool list");
    assert_snapshot!(&result);
}

#[test]
fn test_infer_typing_106() {
    let result = run_type_test("|- let rec map = fun f -> fun l ->
     match l with [] -> [] | x :: y -> f x :: map f y in
     map (fun x -> x < 3) (4 :: 5 :: 1 :: []) : bool list");
    assert_snapshot!(&result);
}
