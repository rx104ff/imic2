use imic2::eval::ast::LanguageVersion;
use imic2::eval::parser::Parser;
use imic2::eval::token;
use insta::assert_snapshot;

fn run_eval_test(version: LanguageVersion, program: &str) -> String {
    let full_input = if version == LanguageVersion::ML1 {
        format!("|- {}", program)
    } else {
        program.to_string()
    };

    let tokens = token::tokenize(&full_input);
    let mut parser = Parser::new(tokens);
    let (env, expr) = parser.parse_program();

    let derivation = imic2::eval::eval::derive(&env, &expr, version);
    format!("{}", derivation)
}


#[test]
fn test_ml1_25() {
    let result = run_eval_test(LanguageVersion::ML1, "3 + 5 evalto 8");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_26() {
    let result = run_eval_test(LanguageVersion::ML1, "8 - 2 - 3 evalto 3");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_27() {
    let result = run_eval_test(LanguageVersion::ML1, "(4 + 5) * (1 - 10) evalto -81");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_28() {
    let result = run_eval_test(LanguageVersion::ML1, "if 4 < 5 then 2 + 3 else 8 * 8 evalto 5");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_29() {
    let result = run_eval_test(LanguageVersion::ML1, "3 + if -23 < -2 * 8 then 8 else 2 + 4 evalto 11");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_30() {
    let result = run_eval_test(LanguageVersion::ML1, "3 + (if -23 < -2 * 8 then 8 else 2) + 4 evalto 15");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_34() {
    let result = run_eval_test(LanguageVersion::ML2, "x = 3, y = 2 |- x evalto 3");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_35() {
    let result = run_eval_test(LanguageVersion::ML2, "x = true, y = 4 |- if x then y + 1 else y - 1 evalto 5");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_36() {
    let result = run_eval_test(LanguageVersion::ML2, "|- let x = 1 + 2 in x * 4 evalto 12");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_37() {
    let result = run_eval_test(LanguageVersion::ML2, "|- let x = 3 * 3 in let y = 4 * x in x + y evalto 45");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_38() {
    let result = run_eval_test(LanguageVersion::ML2, "x = 3 |- let x = x * 2 in x + x evalto 12");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_39() {
    let result = run_eval_test(LanguageVersion::ML2, "|- let x = let y = 3 - 2 in y * y in let y = 4 in x + y evalto 5");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_40() {
    let result = run_eval_test(LanguageVersion::ML3, "|- fun x -> x + 1 evalto ()[fun x -> x + 1]");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_41() {
    let result = run_eval_test(LanguageVersion::ML3, "|- let y = 2 in fun x -> x + y evalto (y=2)[fun x -> x + y]");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_42() {
    let result = run_eval_test(LanguageVersion::ML3, "|- let sq = fun x -> x * x in sq 3 + sq 4 evalto 25");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_43() {
    let result = run_eval_test(LanguageVersion::ML3, "|- let sm = fun f -> f 3 + f 4 in sm (fun x -> x * x) evalto 25");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_44() {
    let result = run_eval_test(LanguageVersion::ML3, "|- let max = fun x -> fun y -> if x < y then y else x in max 3 5 evalto 5");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_45() {
    let result = run_eval_test(LanguageVersion::ML3, "|- let a = 3 in let f = fun y -> y * a in let a = 5 in f 4 evalto 12");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_46() {
    let result = run_eval_test(LanguageVersion::ML3, "|- let twice = fun f -> fun x -> f (f x) in twice (fun x -> x * x) 2 evalto 16");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_47() {
    let result = run_eval_test(LanguageVersion::ML3, "|- let twice = fun f -> fun x -> f (f x) in twice twice (fun x -> x * x) 2 evalto 65536");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_48() {
    let result = run_eval_test(LanguageVersion::ML3, "|- let compose = fun f -> fun g -> fun x -> f (g x) in 
        let p = fun x -> x * x in
        let q = fun x -> x + 4 in
        compose p q 4 
    evalto 64");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_49() {
    let result = run_eval_test(LanguageVersion::ML3, "|- let s = fun f -> fun g -> fun x -> f x (g x) in
        let k = fun x -> fun y -> x in
        s k k 7
    evalto 7");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_50() {
    let result = run_eval_test(LanguageVersion::ML3, "|- let rec fact = fun n ->
        if n < 2 then 1 else n * fact (n - 1) in
        fact 3
    evalto 6");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_51() {
    let result = run_eval_test(LanguageVersion::ML3, "|- let rec fib = fun n -> if n < 3 then 1 else fib (n - 1) + fib (n - 2) in
        fib 5
    evalto 5");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_52() {
    let result = run_eval_test(LanguageVersion::ML3, "|- let rec sum = fun f -> fun n ->
        if n < 1 then 0 else f n + sum f (n - 1) in 
        sum (fun x -> x * x) 2
    evalto 5");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_53() {
    let result = run_eval_test(LanguageVersion::ML3, "|- let fact = fun self -> fun n ->
        if n < 2 then 1 else n * self self (n - 1) in
        fact fact 3
    evalto 6");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_70() {
    let result = run_eval_test(LanguageVersion::ML4, "|- (1 + 2) :: (3 + 4) :: [] evalto 3 :: 7 :: []");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_71() {
    let result = run_eval_test(LanguageVersion::ML4, "|- let f = fun x -> match x with [] -> 0 | a :: b -> a in
        f (4::[]) + f [] + f (1 :: 2 :: 3 :: [])
    evalto 5");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_72() {
    let result = run_eval_test(LanguageVersion::ML4, "|- let rec f = fun x -> if x < 1 then [] else x :: f (x - 1) in
    f 3 evalto 3 :: 2 :: 1 :: []");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_73() {
    let result = run_eval_test(LanguageVersion::ML4, "|- let rec length = fun l -> match l with [] -> 0 | x :: y -> 1 + length y in
    length (1 :: 2 :: 3 :: []) evalto 3");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_74() {
    let result = run_eval_test(LanguageVersion::ML4, "|- let rec length = fun l -> match l with [] -> 0 | x :: y -> 1 + length y in
    length ((1 :: 2 :: []) :: (3 :: 4 :: 5 :: []) :: []) evalto 2");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_75() {
    let result = run_eval_test(LanguageVersion::ML4, "|- let rec append = fun l1 -> fun l2 -> 
      match l1 with [] -> l2 | x :: y -> x :: append y l2 in
    append (1 :: 2 :: []) (3 :: 4 :: 5 :: []) evalto 1 :: 2 :: 3 :: 4 :: 5 :: []");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_76() {
    let result = run_eval_test(LanguageVersion::ML4, "|- let rec apply = fun l -> fun x ->
        match l with [] -> x | f :: l -> f (apply l x) in
        apply ((fun x -> x * x) :: (fun y -> y + 3) :: []) 4 
    evalto 49");
    assert_snapshot!(&result);
}

#[test]
fn test_ml1_77() {
    let result = run_eval_test(LanguageVersion::ML4, "|- let rec apply = fun l -> fun x ->
        match l with [] -> x | f :: l -> apply l (f x) in
        apply ((fun x -> x * x) :: (fun y -> y + 3) :: []) 4 
    evalto 19");
    assert_snapshot!(&result);
}
