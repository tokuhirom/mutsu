//! `methods_narg`: n-argument native method dispatch — facade re-exporting themed submodules.
//!
//! Split from the original monolithic `methods_narg.rs` (3787 lines). Each submodule owns a
//! cohesive group of free functions; this facade preserves every external symbol's original
//! visibility through explicit named re-exports so callers (`methods_narg::<name>`) keep
//! working unchanged. `mod.rs` and other external files are untouched.

mod allomorph;
mod base;
mod buf;
mod dispatch_1arg;
mod dispatch_2arg;
mod flatten;
mod fmt_contains;
mod indent;
mod numeric;
mod str_match;

pub(crate) use dispatch_1arg::native_method_1arg;
pub(crate) use dispatch_2arg::native_method_2arg;
pub(crate) use fmt_contains::{fmt_joinable_target, native_contains_with_options};
pub(crate) use numeric::compute_roots;
pub(crate) use str_match::{native_prefix_suffix_with_options, native_substr_eq_with_options};
