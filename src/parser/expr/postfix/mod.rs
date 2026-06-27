//! Postfix expression parsing: facade re-exporting themed submodules.
//!
//! The original monolithic `postfix.rs` has been split into:
//! - `helpers`:    Small utility functions (negative literal detection, angle key chars, etc.)
//! - `adverb`:     Subscript adverb parsing (`:exists`, `:delete`, `:k`, `:kv`, etc.)
//! - `call_method`: Method call helpers (bracket indices, quoted method names, postfix operators)
//! - `dot_assign`: `.=` mutating method call parsing
//! - `loop_`:      Main postfix/prefix expression loops (`prefix_expr`, `postfix_expr`, etc.)

mod adverb;
mod call_method;
mod dot_assign;
mod helpers;
mod loop_;

// Re-exports preserving original visibility
pub(in crate::parser::expr) use loop_::{postfix_expr_tight_pub, prefix_expr};

pub(crate) use call_method::{QuotedMethodName, parse_quoted_method_name};
pub(in crate::parser) use loop_::postfix_expr_continue;
