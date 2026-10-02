//! Regex capture data — what a match leaves behind and what a lazy `Match`
//! value reads: the accumulator (`RegexCaptures`), the stored nodes
//! (`CapNode`, `PosSlot`, `NamedCaptureMap`) and the shared subject
//! (`MatchTarget`). Moved below the runtime (from `runtime/regex_types.rs`,
//! `runtime/regex_named_caps.rs` and `runtime/match_target.rs`) so `Value`
//! does not name the runtime for them (#10779); the runtime re-exports the
//! whole set, so engine code keeps its `crate::runtime::…` paths.

mod cap_node;
mod captures;
mod marks;
mod match_target;
mod named_caps;
pub(crate) mod stats;

pub(crate) use cap_node::{
    CapChildren, CapNode, OuterBackrefCaps, PosSlot, QuantifiedCaptureEntry,
    SILENT_ACTION_MARKER_PREFIX,
};
pub(crate) use captures::{CaptureAliasMap, RegexCaptures};
pub(crate) use marks::strip_marks_text;
pub(crate) use match_target::MatchTarget;
pub(crate) use named_caps::*;

/// The `:my $var = …` regex-variable map shape, Fx-hashed for the same
/// reason as the regex capture maps.
pub(crate) type RegexVarMap = rustc_hash::FxHashMap<String, crate::value::Value>;
