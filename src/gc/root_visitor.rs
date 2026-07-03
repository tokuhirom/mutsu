//! GC Level 1a root-visitor infrastructure (ADR-0001 / ADR-0002,
//! `docs/gc-level1-detailed-design.md` §2.2 / §11 steps 1-2).
//!
//! This submodule holds the `RootVisitor` trait and its `visit_*` helpers. Root
//! enumeration (`Interpreter::visit_roots`, `Env::visit_values`) lives in
//! exactly one place through this trait instead of being duplicated per future
//! call site. The managed-pointer machinery (`Gc<T>`, candidate buffer, the
//! future collector) lives in the sibling `gc_ptr` submodule.

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
