//! GC Level 1a infrastructure (ADR-0001 / ADR-0002,
//! `docs/gc-level1-detailed-design.md`).
//!
//! - The `RootVisitor` trait (below) is the single place root enumeration lives
//!   (`Interpreter::visit_roots`, `Env::visit_values`, `Value::visit_gc_children`).
//! - The [`collect`] submodule (§11 step 4) adds the `Gc<T>` smart pointer, the
//!   node header, the candidate buffer, and a synchronous Bacon–Rajan cycle
//!   collector. No `Value` variant is wired to `Gc<T>` yet (that is step 5), so
//!   the collector is currently exercised only by its own unit tests.

mod collect;

// Re-exported for the step-5+ callers (Value migration, safepoint collect).
// Unused until then, hence `allow(unused_imports)`.
#[allow(unused_imports)]
pub(crate) use collect::{Gc, Trace, collect_cycles};

use std::collections::HashMap;
use std::hash::BuildHasher;

use crate::value::Value;

/// Callback invoked once per `Value` reachable from a GC root.
///
/// A plain (non-generic) trait so `&mut dyn RootVisitor` can be threaded
/// through root enumeration without monomorphizing it per visitor type. The
/// `visit_*` free functions below cover the shapes roots are commonly stored
/// in (`Option<Value>`, `&[Value]`, `HashMap<_, Value>`) — they take `&mut dyn
/// RootVisitor` rather than being trait default methods because a generic
/// method (needed for the `HashMap`'s hasher parameter) would make the trait
/// object-unsafe.
///
/// No implementor exists yet outside `#[cfg(test)]`: this is GC Level 1a step
/// 1 (root visitor only, `docs/gc-level1-detailed-design.md` §11). The
/// candidate-buffer/collector implementor lands in step 4, at which point
/// these become live production callers — see ADR-0001/ADR-0002.
#[allow(dead_code)]
pub(crate) trait RootVisitor {
    fn visit_value(&mut self, value: &Value);
}

#[allow(dead_code)]
pub(crate) fn visit_opt(visitor: &mut dyn RootVisitor, value: &Option<Value>) {
    if let Some(v) = value {
        visitor.visit_value(v);
    }
}

#[allow(dead_code)]
pub(crate) fn visit_slice(visitor: &mut dyn RootVisitor, values: &[Value]) {
    for v in values {
        visitor.visit_value(v);
    }
}

#[allow(dead_code)]
pub(crate) fn visit_map_values<K, S: BuildHasher>(
    visitor: &mut dyn RootVisitor,
    map: &HashMap<K, Value, S>,
) {
    for v in map.values() {
        visitor.visit_value(v);
    }
}
