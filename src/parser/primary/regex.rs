//! Regex/subst/version/topic-method primary parsing: facade re-exporting themed submodules.
//!
//! Split from the original monolithic `regex.rs`. Each submodule owns a
//! cohesive group of free functions; this facade preserves every function's
//! original visibility through explicit named re-exports so external callers
//! (`regex::<name>`) keep working unchanged.
//!
//! Submodules:
//!   - `trans`      (63 lines)  — tr/// escape processing + adverb parsing
//!   - `adverbs`    (310 lines) — MatchAdverbs struct + all adverb parsing/building
//!   - `subst`      (217 lines) — s/// / S/// building helpers
//!   - `scan`       — regex delimiter scanning and embedded-code helpers
//!   - `call_args`  (140 lines) — parse_call_arg_list + colon-method-arg helpers
//!   - `lit`        (~1060 lines) — regex_lit (~800 ln, indivisible), version_lit, topic_method_call

mod adverbs;
mod call_args;
mod lit;
mod rakuast_adverbs;
mod scan;
mod subst;
mod subst_source;
mod trans;

// Items used by external callers inside crate::parser (comparison.rs, token_body.rs,
// reduction.rs, ident.rs, primary/mod.rs).  Visibility matches the original regex.rs.
pub(in crate::parser) use call_args::{parse_call_arg_list, sigilless_item_assign_arg};
pub(in crate::parser) use lit::regex_lit;
pub(in crate::parser) use scan::scan_to_delim;
pub(crate) use subst_source::{SubstFlags, parse_adverb_argument, subst_pattern_source};

// Items used by primary/mod.rs (sibling module).
pub(super) use lit::{topic_method_call, version_lit};
