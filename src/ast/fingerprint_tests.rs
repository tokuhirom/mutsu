//! Pins for the AST identity fingerprints
//! ([#7822](https://github.com/tokuhirom/mutsu/issues/7822)).
//!
//! The three fingerprints moved from hashing a `Debug` rendering of the AST to
//! hashing it structurally through the derived `Hash` impls. The two properties
//! that survived that move are the ones worth pinning: the line-sensitivity
//! split between the two entry points, and the fact that a fingerprint depends
//! on structure alone, so two separately-built copies of one declaration agree.

use super::*;
use crate::value::Value;

fn say(text: &str) -> Stmt {
    Stmt::Say(vec![Expr::Literal(Value::str(text.to_string()))])
}

fn param(name: &str) -> ParamDef {
    ParamDef {
        name: name.to_string(),
        default: None,
        multi_invocant: true,
        required: true,
        named: false,
        named_alias: false,
        slurpy: false,
        double_slurpy: false,
        onearg: false,
        sigilless: false,
        type_constraint: None,
        literal_value: None,
        sub_signature: None,
        where_constraint: None,
        traits: Vec::new(),
        optional_marker: false,
        outer_sub_signature: None,
        code_signature: None,
        is_invocant: false,
        shape_constraints: None,
        block_param: false,
    }
}

/// A fingerprint is a pure function of structure, so a second, separately
/// built copy of the same declaration agrees with the first. This is what lets
/// multi-candidate identity and redeclaration comparison work across two
/// parses of one source.
#[test]
fn structurally_identical_declarations_agree() {
    let build = || {
        (
            vec!["$a".to_string()],
            vec![param("$a")],
            vec![Stmt::SetLine(7), say("hi")],
        )
    };
    let (p1, d1, b1) = build();
    let (p2, d2, b2) = build();

    assert_eq!(
        function_body_fingerprint(&p1, &d1, &b1),
        function_body_fingerprint(&p2, &d2, &b2)
    );
    assert_eq!(
        registration_identity_fingerprint(&p1, &d1, &b1),
        registration_identity_fingerprint(&p2, &d2, &b2)
    );
}

/// `function_body_fingerprint` hashes `SetLine` like any other statement;
/// `registration_identity_fingerprint` strips top-level ones so a
/// redeclaration that moved down the file still compares equal.
#[test]
fn only_the_registration_identity_is_line_insensitive() {
    let params: Vec<String> = Vec::new();
    let defs: Vec<ParamDef> = Vec::new();
    let at_line_3 = vec![Stmt::SetLine(3), say("body")];
    let at_line_9 = vec![Stmt::SetLine(9), say("body")];

    assert_ne!(
        function_body_fingerprint(&params, &defs, &at_line_3),
        function_body_fingerprint(&params, &defs, &at_line_9),
        "the structural fingerprint must stay line-sensitive"
    );
    assert_eq!(
        registration_identity_fingerprint(&params, &defs, &at_line_3),
        registration_identity_fingerprint(&params, &defs, &at_line_9),
        "the redeclaration identity must ignore top-level SetLine markers"
    );
}

/// Neither entry point may collapse a genuine difference in the body.
#[test]
fn a_different_body_fingerprints_differently() {
    let params: Vec<String> = Vec::new();
    let defs: Vec<ParamDef> = Vec::new();
    let a = vec![say("a")];
    let b = vec![say("b")];

    assert_ne!(
        function_body_fingerprint(&params, &defs, &a),
        function_body_fingerprint(&params, &defs, &b)
    );
    assert_ne!(
        registration_identity_fingerprint(&params, &defs, &a),
        registration_identity_fingerprint(&params, &defs, &b)
    );
}

/// Statement *count* is part of the identity: appending a statement changes
/// the fingerprint even where the extra statement is a `SetLine`-free no-op.
/// This is what the explicit count guard in
/// `registration_identity_fingerprint` buys, standing in for the length prefix
/// a slice hash would have written.
#[test]
fn appending_a_statement_changes_the_registration_identity() {
    let params: Vec<String> = Vec::new();
    let defs: Vec<ParamDef> = Vec::new();
    let one = vec![say("a")];
    let two = vec![say("a"), say("a")];

    assert_ne!(
        registration_identity_fingerprint(&params, &defs, &one),
        registration_identity_fingerprint(&params, &defs, &two)
    );
}

/// The three hashed fields must not bleed into one another: moving a name from
/// the parameter-name list into a `ParamDef` is a different declaration.
#[test]
fn params_and_param_defs_are_separate_fields() {
    let body: Vec<Stmt> = Vec::new();
    let as_names = function_body_fingerprint(&["$a".to_string()], &[], &body);
    let as_defs = function_body_fingerprint(&[], &[param("$a")], &body);
    assert_ne!(as_names, as_defs);
}

/// `sub_registration_fingerprint` extends the body fingerprint with the flags
/// that distinguish otherwise same-bodied declarations.
#[test]
fn sub_registration_fingerprint_separates_the_declaration_flags() {
    let params: Vec<String> = Vec::new();
    let defs: Vec<ParamDef> = Vec::new();
    let body = vec![say("a")];
    let plain = sub_registration_fingerprint(&params, &defs, &body, None, false, false, false);
    let multi = sub_registration_fingerprint(&params, &defs, &body, None, true, false, false);
    let rw = sub_registration_fingerprint(&params, &defs, &body, None, false, true, false);
    let returns = sub_registration_fingerprint(
        &params,
        &defs,
        &body,
        Some(&"Int".to_string()),
        false,
        false,
        false,
    );

    assert_ne!(plain, multi);
    assert_ne!(plain, rw);
    assert_ne!(plain, returns);
}

/// A literal's identity is its *declaration* shape, not its Raku value: `1`
/// and `1.0` are equal under `PartialEq for Value` but are different source
/// texts, so two routines returning them are different declarations.
#[test]
fn literals_hash_by_declaration_identity_not_value_equality() {
    let params: Vec<String> = Vec::new();
    let defs: Vec<ParamDef> = Vec::new();
    let int = vec![Stmt::Say(vec![Expr::Literal(Value::int(1))])];
    let num = vec![Stmt::Say(vec![Expr::Literal(Value::num(1.0))])];

    assert_eq!(Value::int(1), Value::num(1.0));
    assert_ne!(
        function_body_fingerprint(&params, &defs, &int),
        function_body_fingerprint(&params, &defs, &num)
    );
}
