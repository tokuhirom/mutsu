//! Container expression parsing: facade re-exporting themed submodules.
//!
//! Split from the original monolithic `container.rs` (1887 lines). Each submodule
//! owns a cohesive group of free functions; this facade preserves every function's
//! original visibility through explicit re-exports so external callers
//! (`container::<name>`) keep working unchanged. `primary/mod.rs` is untouched.
//!
//! - allomorph.rs (279)  - allomorphic value construction from angle-word strings
//! - angle_words.rs (357) - angle/french/double-angle quote-word list parsing
//! - array.rs (215)      - array composer `[...]`, `%(...)`, error helpers
//! - meta_ops.rs (354)   - zip/meta-op normalization helpers for paren lists
//! - paren.rs (498)      - core parenthesized expression / list parsing
//! - sigil_context.rs (179) - itemized/context forms: `$()`, `@()`, `%()`, `$[]`, `${}`

mod allomorph;
mod angle_words;
mod array;
mod meta_ops;
mod paren;
mod sigil_context;

// pub(super) re-exports — visible within crate::parser::primary and its submodules
pub(super) use angle_words::{
    angle_list, double_angle_list, find_nested_angle_close_pub, french_quote_list,
};
pub(super) use array::{array_literal, fail_goal_error, percent_hash_literal};
pub(super) use paren::paren_expr;
pub(super) use sigil_context::{
    hash_context_paren_expr, itemized_brace_expr, itemized_bracket_expr,
    itemized_context_paren_expr, itemized_paren_expr, list_context_paren_expr,
};

// pub(crate) re-exports — visible throughout the crate
pub(crate) use allomorph::{angle_word_value, angle_word_value_full_allomorphic};
