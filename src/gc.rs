//! GC Level 1a root-visitor infrastructure (ADR-0001 / ADR-0002,
//! `docs/gc-level1-detailed-design.md`).
//!
//! This module holds only the `RootVisitor` trait so far. It has no
//! knowledge of `Gc<T>`, candidate buffers, or collection — those land in
//! later steps of the design doc's §11 implementation order. The trait exists
//! now so root enumeration lives in exactly one place (`Interpreter::visit_roots`,
//! `Env::visit_values`) instead of being duplicated per future call site.

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
