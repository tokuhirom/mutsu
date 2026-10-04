//! A module AST read back from the cache must be the AST a fresh parse
//! produces (ADR-11756 §2.3): the compiled-bytecode cache stores code compiled
//! from one and runs it next to the other. Process-global counters minting into
//! the AST (declaration ids, anonymous type names, END indices) broke that.

use std::hash::{Hash, Hasher};

fn parse_module(src: &str) -> Vec<crate::ast::Stmt> {
    let session = crate::compiler::compile_session::content_parse_session_id(0x5eed, 0);
    crate::anon_names::with_content_unit(session, || {
        crate::parse_dispatch::parse_compilation_unit_of(
            src,
            crate::rakuast::frontend::Unit::Module,
        )
    })
    .expect("module parses")
    .0
}

fn assert_round_trips(src: &str) {
    let stmts = parse_module(src);
    let bytes =
        bincode::serde::encode_to_vec(&stmts, bincode::config::standard()).expect("encodes");
    let (back, _): (Vec<crate::ast::Stmt>, usize) =
        bincode::serde::decode_from_slice(&bytes, super::decode_config()).expect("decodes");
    let hash = |s: &crate::ast::Stmt| {
        let mut h = std::hash::DefaultHasher::new();
        s.hash(&mut h);
        h.finish()
    };
    assert_eq!(stmts.len(), back.len());
    for (i, (a, b)) in stmts.iter().zip(&back).enumerate() {
        assert_eq!(format!("{a:?}"), format!("{b:?}"), "statement {i} differs");
        assert_eq!(hash(a), hash(b), "statement {i} hashes differently");
    }
}

#[test]
fn declarations_anonymous_types_and_end_phasers_survive_the_cache() {
    assert_round_trips(
        "unit module M;\n\
         my class Local { has $.x }\n\
         my role R { method r { 1 } }\n\
         our $anon = class { has $.y };\n\
         our $anon-role = role { };\n\
         END { say 'bye' }\n\
         sub f is export { Local.new(x => 1).x }\n",
    );
}

#[test]
fn the_bundled_test_module_survives_the_cache() {
    let path = concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/modules/Rakudo-Core/lib/Test.rakumod"
    );
    assert_round_trips(&std::fs::read_to_string(path).expect("Test.rakumod is readable"));
}
