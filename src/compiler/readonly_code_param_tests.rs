use super::Compiler;

/// The `readonly_code_params` of the sub `name` declared in `src`; empty when
/// the source does not parse or the sub is not compiled, which fails the
/// assertions below.
fn readonly_params(src: &str, name: &str) -> Vec<String> {
    let Ok((stmts, _)) = crate::parse_dispatch::parse_source(src) else {
        return vec!["<does not parse>".to_string()];
    };
    let (code, fns) = Compiler::new().compile(&stmts);
    let function = code
        .sub_decl_plans
        .iter()
        .find(|plan| plan.name.as_str() == name && !plan.compiled_routine_keys.is_empty())
        .and_then(|plan| fns.get(&plan.compiled_routine_keys[0]));
    let Some(function) = function else {
        return vec!["<not compiled>".to_string()];
    };
    let mut names: Vec<String> = function
        .code
        .readonly_code_params
        .iter()
        .map(|s| s.as_str().to_string())
        .collect();
    names.sort();
    names
}

#[test]
fn plain_scalar_and_callable_parameters_are_readonly() {
    let names = readonly_params("sub f($id, &cb, :$n) { }; f(1, {})", "f");
    assert_eq!(names, ["&cb", "id", "n"]);
}

#[test]
fn copy_rw_and_raw_parameters_are_not_readonly() {
    let names = readonly_params(
        "sub f($a is copy, $b is rw, $c is raw, &d is copy, $e) { }; f(1, 2, 3, {}, 5)",
        "f",
    );
    assert_eq!(names, ["e"]);
}

#[test]
fn aggregate_and_slurpy_parameters_are_not_readonly() {
    let names = readonly_params("sub f($x, @a, %h, *@rest) { }; f(1, [], {}, 2)", "f");
    assert_eq!(names, ["x"]);
}
