//! Attributes an instance computes on first access instead of at construction.
//!
//! Some objects are built on a hot path but read rarely: the `Backtrace` a
//! `die` attaches to its exception is rendered from the whole call stack, yet a
//! `try { die }` that never asks for `.backtrace` throws it away unread. Such an
//! instance is created with a [`LazyAttrSource`] instead of those attributes,
//! and the first access to its attribute map asks the source for them (see
//! `InstanceAttrs::cell`). After that the instance is an ordinary one.
//!
//! The source must answer the same attributes whenever it is asked, and must
//! hold no `Value` itself: the GC traces an instance's attribute map, which
//! does not yet hold the attributes a pending source will produce.

use super::Value;
use std::sync::Arc;

/// Produces the attributes of an instance that were deferred at construction.
pub(crate) trait LazyAttrSource: Send + Sync + std::fmt::Debug {
    /// The deferred `(name, value)` pairs. Called at most once per instance;
    /// a pair whose name the map already holds is ignored.
    fn materialize(&self) -> Vec<(&'static str, Value)>;
}

/// The per-object side state an instance shares with every alias of itself
/// (a reblessed or mixed-in view shares it along with the attribute cell).
#[derive(Debug, Default)]
pub(crate) struct InstanceSide {
    /// See `InstanceAttrs::which_memo`.
    pub(crate) which: Option<Arc<str>>,
    /// The not-yet-materialized attributes, if any.
    pub(crate) lazy: Option<Arc<dyn LazyAttrSource>>,
}
