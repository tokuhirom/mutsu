//! A dispatcher code value captures its candidates in declaration order
//! (#18043), whatever order the gather handed them in.

use super::*;

/// The first parameter type of each captured candidate, first copy only.
fn captured_param_types(dispatcher: &Value) -> Vec<String> {
    let ValueView::Sub(data) = dispatcher.view() else {
        panic!("a dispatcher is a code value");
    };
    let mut seen = Vec::new();
    for cand in Interpreter::captured_dispatch_candidates(&data).expect("captured candidates") {
        let ValueView::Sub(cand) = cand.view() else {
            continue;
        };
        let ty = cand
            .param_defs
            .first()
            .and_then(|p| p.type_constraint.clone())
            .unwrap_or_default();
        if !seen.contains(&ty) {
            seen.push(ty);
        }
    }
    seen
}

#[test]
fn captured_candidates_are_in_declaration_order() {
    let mut i = Interpreter::new();
    i.run("multi f(Int $x) { 1 }\nmulti f(Str $x) { 2 }\nmulti f(Num $x) { 3 }\n")
        .expect("setup program runs");
    let mut defs: Vec<std::sync::Arc<crate::runtime::FunctionDef>> = i
        .routines()
        .functions
        .iter()
        .filter(|(key, _)| key.as_str().starts_with("GLOBAL::f/"))
        .map(|(_, def)| def.clone())
        .collect();
    assert!(defs.len() >= 3, "the three candidates are registered");
    // Hand them over newest first, the worst order a gather can produce.
    defs.sort_by_key(|def| std::cmp::Reverse(def.decl_order));
    let dispatcher = i.sub_value_from_multi_candidates("f", defs);
    assert_eq!(captured_param_types(&dispatcher), ["Int", "Str", "Num"]);
}
