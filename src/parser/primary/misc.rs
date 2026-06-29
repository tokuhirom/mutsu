//! Misc primary parsing: facade re-exporting themed submodules.
//!
//! Split from the original monolithic `misc.rs`. Each submodule owns a
//! cohesive group of free functions; this facade preserves every function's
//! original visibility through explicit named re-exports so external callers
//! (`misc::<name>`) keep working unchanged.

mod anon_decl;
mod colonpair;
mod hash;
mod lambda;
mod reduction;

pub(super) use anon_decl::{anon_class_expr, anon_grammar_expr, anon_role_expr};
pub(super) use lambda::{arrow_lambda, capture_literal};
pub(super) use reduction::reduction_op;

pub(in crate::parser) use colonpair::{colonpair_expr, wrap_colonpair_sink_source};
pub(in crate::parser) use lambda::{block_or_hash_expr, parse_block_body};
pub(in crate::parser) use reduction::reduction_call_style_expr;

/// Simple whitespace consumer that doesn't use PResult (infallible).
pub(super) fn ws_inner(input: &str) -> (&str, ()) {
    match crate::parser::helpers::ws(input) {
        Ok((r, _)) => (r, ()),
        Err(_) => (input, ()),
    }
}
