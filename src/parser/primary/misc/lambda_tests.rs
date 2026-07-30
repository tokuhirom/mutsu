//! Unit tests for the `{ … }` hash-composer-vs-block decision in `lambda.rs`.
//!
//! The decision is a lexical heuristic, so it is tested here on the parsed AST
//! rather than through the interpreter: a wrong answer silently turns a hash
//! literal into a closure (or vice versa), which is far easier to read off
//! `Expr::Hash` / `Expr::AnonSub` than off a program's output.

use crate::ast::Expr;
use crate::parser::primary::misc::block_or_hash_expr;

/// `true` if the source parses as a hash literal, `false` if as a block.
/// Panics if it does not parse at all.
fn is_hash(src: &str) -> bool {
    let (rest, expr) = block_or_hash_expr(src).unwrap_or_else(|e| panic!("{src:?}: {e:?}"));
    assert_eq!(rest, "", "{src:?} left unparsed input");
    match expr {
        Expr::Hash(_) => true,
        // A body that mixes pairs with a spread compiles to a `hash(...)` call
        // rather than an `Expr::Hash`; still the hash-composer reading.
        Expr::Call { ref name, .. } => name.resolve() == "hash",
        _ => false,
    }
}

#[test]
fn plain_pair_bodies_are_hashes() {
    assert!(is_hash("{ a => 1 }"));
    assert!(is_hash("{ :type<var>, :val('v') }"));
    assert!(is_hash("{}"));
}

#[test]
fn topic_references_force_a_block() {
    // An explicit topic variable, in the key or the value.
    assert!(!is_hash("{ a => $_ }"));
    assert!(!is_hash(r#"{ "$_" => 1 }"#));
    // An invocant-less method call is a topic reference too.
    assert!(!is_hash("{ a => .key }"));
    assert!(!is_hash("{ .key => 1 }"));
    // Infix division by a topic method call — the one `/`-before-`.` spelling
    // that is NOT a term end (see `is_implicit_topic_call`).
    assert!(!is_hash("{ a => 1 / .elems }"));
    // ...and prefix negation of one.
    assert!(!is_hash("{ a => !.defined }"));
}

#[test]
fn punctuation_variables_are_terms_not_topic_calls() {
    // `$/` and `$!` end in punctuation, but `.from` / `.message` still has a
    // real invocant. Regression: `make { :pos($/.from) }` in a grammar action
    // parsed as a block, so the action produced a Callable instead of a Hash.
    assert!(is_hash("{ :pos($/.from) }"));
    assert!(is_hash("{ a => $/.from, b => 1 }"));
    assert!(is_hash("{ a => $!.message }"));
    assert!(is_hash("{ a => $¢.pos }"));
}

#[test]
fn a_closing_quote_delimiter_is_a_term_end() {
    // The `/` that closes a quoting construct or a regex literal ends a term,
    // so the following `.method` has an invocant. The heredoc spelling that
    // triggered this (`b => q:to/EOF/.trim,`) needs its body on the following
    // lines, so it is pinned end-to-end in `t/hash-composer-term-end.t`.
    assert!(is_hash("{ a => q/x/.uc }"));
    assert!(is_hash("{ a => /rx/.gist }"));
}

#[test]
fn method_calls_with_ordinary_invocants_stay_hashes() {
    assert!(is_hash("{ a => $x.key }"));
    assert!(is_hash("{ a => %h<k>.key }"));
    assert!(is_hash("{ a => (1, 2).elems }"));
    // A `.^name` inside a string interpolation belongs to the interpolation's
    // own closure, so the outer literal is still a hash.
    assert!(is_hash(r#"{ "{ .^name }X" => 1 }"#));
}

#[test]
fn placeholder_variables_force_a_block() {
    assert!(!is_hash("{ a => $^x }"));
}
