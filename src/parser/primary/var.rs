//! Variable parsing facade: re-exporting themed submodules.
//!
//! Split from the original monolithic `var.rs` (1329 lines). Each submodule
//! owns a cohesive group of free functions; this facade preserves every
//! function's original visibility through explicit named re-exports so external
//! callers (`var::<name>` and `crate::parser::primary::var::<name>`) keep
//! working unchanged.  `primary/mod.rs` is untouched.
//!
//! - ident.rs      (138)  — identifier + pseudo-package utilities
//! - adverb.rs     (113)  — adverb-suffix + bracket-value parsing
//! - perl5.rs      (172)  — Perl 5 compatibility detection
//! - scalar.rs     (292)  — $-sigil parser + dollar-paren-block
//! - sigil_vars.rs (399)  — @/%/& sigil parsers + operator code-ref

mod adverb;
mod ident;
mod perl5;
mod scalar;
mod sigil_vars;

// ── pub(crate): accessible from anywhere in the crate ──────────────────────
pub(crate) use adverb::parse_adverb_value_pub;
pub(crate) use ident::{is_pseudo_package, parse_ident_with_hyphens};
pub(crate) use perl5::detect_perl5_scalar_var;

// ── pub(super): accessible from parser::primary (the parent of `var`)
//    and all its descendants via `crate::parser::primary::var::` paths ──────
pub(super) use scalar::{parse_dollar_paren_block_pub, parse_var_name_from_str, scalar_var};
pub(super) use sigil_vars::{array_var, code_var, hash_var};
