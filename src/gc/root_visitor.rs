//! GC Level 1a root-visitor infrastructure (ADR-0001 / ADR-0002,
//! `docs/gc-level1-detailed-design.md` §2.2 / §11 steps 1-2).
//!
//! This submodule holds the `RootVisitor` trait and its `visit_*` helpers. Root
//! enumeration (`Interpreter::visit_roots`, `Env::visit_values`) lives in
//! exactly one place through this trait instead of being duplicated per
//! call site. The managed-pointer machinery (`Gc<T>`, candidate buffer,
//! collector) lives in the sibling `gc_ptr` / `collect` submodules.
//!
//! The shipped collector is candidate-buffer Bacon-Rajan and does NOT consume
//! roots, so this whole surface is exercised only by the `gc_roots.rs` unit
//! tests today (hence the `#[allow(dead_code)]`s). It is retained deliberately:
//! the H4 hardening item (full-root VERIFY mode, `docs/gc-post-3a-roadmap.md`
//! §1) and any future tracing/moving design need exactly this enumeration, and
//! keeping it compiling keeps root coverage honest as interpreter state grows.

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
/// No implementor exists outside `#[cfg(test)]` — see the module doc for why
/// this infrastructure is retained without a production consumer.
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
