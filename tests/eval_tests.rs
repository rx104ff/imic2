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
