use pretty_assertions::assert_eq;

fn fixture(name: &str) -> (String, String) {
    let source = std::fs::read_to_string(format!("examples/{name}.ob")).unwrap();
    let expected = std::fs::read_to_string(format!("fixtures/{name}.rb")).unwrap();

    (source, expected)
}

fn compile_eq(name: &str) {
    let (source, expected) = fixture(name);

    let actual = oxiby::compile_str(&[name], &source).unwrap();

    assert_eq!(actual.trim_end(), expected.trim_end());
}

macro_rules! test_fixture {
    ($func:ident, $fixture:literal) => {
        #[test]
        fn $func() {
            compile_eq($fixture);
        }
    };
}

test_fixture!(call, "call");
test_fixture!(closures, "closures");
test_fixture!(conditional, "conditional");
test_fixture!(container, "container");
test_fixture!(enums, "enums");
test_fixture!(fn_param_combos, "fn_param_combos");
test_fixture!(functions, "functions");
test_fixture!(lists, "lists");
test_fixture!(loops, "loops");
test_fixture!(maps, "maps");
test_fixture!(patterns, "patterns");
test_fixture!(result_tuple, "result_tuple");
test_fixture!(structs, "structs");
test_fixture!(tic_tac_toe, "tic_tac_toe");
test_fixture!(tour, "tour");
test_fixture!(tuple_access, "tuple_access");
test_fixture!(types, "types");
