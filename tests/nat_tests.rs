use imic2::{common, nat};
use imic2::nat::version::ComparisonMode;
use imic2::nat::parser::Parser;

use insta::assert_snapshot;


fn run_nat_test(mode: Option<ComparisonMode>, input: &str) -> String {
    let tokens = common::tokenizer::tokenize(input);
    let mut parser = Parser::new(tokens);
    let judgment = match parser.parse_judgment() {
        Ok(j) => j,
        Err(e) => return format!("Parsing Error: {}", e),
    };

    match nat::nat::derive(&judgment, mode) {
        Ok(derivation) => format!("{}", derivation),
        Err(e) => format!("Derivation Error: {}", e),
    }
}

#[test]
fn test_nat_1() {
    let result = run_nat_test(None, "Z plus Z is Z");
    assert_snapshot!(result);
}

#[test]
fn test_nat_2() {
    let result = run_nat_test(None, "Z plus S(S(Z)) is S(S(Z))");
    assert_snapshot!(result);
}

#[test]
fn test_nat_3() {
    let result = run_nat_test(None, "S(S(Z)) plus Z is S(S(Z))");
    assert_snapshot!(result);
}

#[test]
fn test_nat_4() {
    let result = run_nat_test(None, "S(Z) plus S(S(S(Z))) is S(S(S(S(Z))))");
    assert_snapshot!(result);
}

#[test]
fn test_nat_5() {
    let result = run_nat_test(None, "Z times S(S(Z)) is Z");
    assert_snapshot!(result);
}

#[test]
fn test_nat_6() {
    let result = run_nat_test(None, "S(S(Z)) times Z is Z");
    assert_snapshot!(result);
}

#[test]
fn test_nat_7() {
    let result = run_nat_test(None, "S(S(Z)) times S(Z) is S(S(Z))");
    assert_snapshot!(result);
}

#[test]
fn test_nat_8() {
    let result = run_nat_test(None, "S(S(Z)) times S(S(Z)) is S(S(S(S(Z))))");
    assert_snapshot!(result);
}

#[test]
fn test_compare_nat_1_9() {
    let result = run_nat_test(Some(ComparisonMode::V1), "S(S(Z)) is less than S(S(S(Z)))");
    assert_snapshot!(result);
}

#[test]
fn test_compare_nat_2_10() {
    let result = run_nat_test(Some(ComparisonMode::V2), "S(S(Z)) is less than S(S(S(Z)))");
    assert_snapshot!(result);
}

#[test]
fn test_compare_nat_3_11() {
    let result = run_nat_test(Some(ComparisonMode::V3), "S(S(Z)) is less than S(S(S(Z)))");
    assert_snapshot!(result);
}

#[test]
fn test_compare_nat_3_12() {
    let result = run_nat_test(Some(ComparisonMode::V1), "S(S(Z)) is less than S(S(S(S(S(Z)))))");
    assert_snapshot!(result);
}

#[test]
fn test_compare_nat_3_13() {
    let result = run_nat_test(Some(ComparisonMode::V2), "S(S(Z)) is less than S(S(S(S(S(Z)))))");
    assert_snapshot!(result);
}

#[test]
fn test_compare_nat_3_14() {
    let result = run_nat_test(Some(ComparisonMode::V3), "S(S(Z)) is less than S(S(S(S(S(Z)))))");
    assert_snapshot!(result);
}

#[test]
fn test_eval_nat_15() {
    let result = run_nat_test(None, "Z + S(S(Z)) evalto S(S(Z))");
    assert_snapshot!(result);
}

#[test]
fn test_eval_nat_16() {
    let result = run_nat_test(None, "S(S(Z)) + Z evalto S(S(Z))");
    assert_snapshot!(result);
}

#[test]
fn test_eval_nat_17() {
    let result = run_nat_test(None, "S(Z) + S(Z) + S(Z) evalto S(S(S(Z)))");
    assert_snapshot!(result);
}

#[test]
fn test_eval_nat_18() {
    let result = run_nat_test(None, "S(S(S(Z))) + S(S(Z)) * S(Z) evalto S(S(S(S(S(Z)))))");
    assert_snapshot!(result);
}

#[test]
fn test_eval_nat_19() {
    let result = run_nat_test(None, "(S(S(Z)) + S(S(Z))) * Z evalto Z");
    assert_snapshot!(result);
}

#[test]
fn test_eval_nat_20() {
    let result = run_nat_test(None, "Z * (S(S(Z)) + S(S(Z))) evalto Z");
    assert_snapshot!(result);
}

#[test]
fn test_reduce_nat_21() {
    let result = run_nat_test(None, "Z + S(S(Z)) -*-> S(S(Z))");
    assert_snapshot!(result);
}

#[test]
fn test_reduce_nat_22() {
    let result = run_nat_test(None, "S(Z) * S(Z) + S(Z) * S(Z) -d-> S(Z) + S(Z) * S(Z)");
    assert_snapshot!(result);
}

#[test]
fn test_reduce_nat_23() {
    let result = run_nat_test(None, "S(Z) * S(Z) + S(Z) * S(Z) ---> S(Z) * S(Z) + S(Z)");
    assert_snapshot!(result);
}

#[test]
fn test_reduce_nat_24() {
    let result = run_nat_test(None, "S(Z) * S(Z) + S(Z) * S(Z) -*-> S(S(Z))");
    assert_snapshot!(result);
}
