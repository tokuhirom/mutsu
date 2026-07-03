//! GC Level 1a child visitor (ADR-0001/0002,
//! `docs/gc-level1-detailed-design.md` §3 / §7 / §11 step 2).
//!
//! Traces the direct `Value` children of a GC-candidate node. This is the
//! collector's "trace edges out of this node" primitive, distinct from
//! `Interpreter::visit_roots` (which enumerates ROOTS — where execution state
//! itself holds a `Value`). No `Gc<T>`/candidate buffer exists yet, so
//! nothing outside this module's tests calls these.
//!
//! Scope matches the design doc's §3.1 type filter plus the "initial scope"
//! async types: `Array`/`Hash`/`Set`/`Bag`/`Mix`/`Pair`/`ValuePair`/`Sub`/
//! `Instance`/`ContainerRef`/`Promise`/`Channel`. Every other variant
//! (`Int`/`Str`/`Bool`/`Nil`/`Range`/...) cannot form a cycle and is a no-op.
//! `LazyList` is out of scope until the third wave (§11 step 10) — its wide
//! trace surface (env/cache/coroutine/lazy-pipe state) is deferred there.
//! `Junction` is likewise not in the design doc's candidate list and is
//! excluded.

use crate::gc::{ErasedGc, RootVisitor, Trace, visit_map_values};

use super::{
    ArrayData, BagData, HashData, InstanceAttrs, MixData, SetData, SharedChannel, SharedPromise,
    SubData, Value,
};

impl Value {
    /// Hand each direct GC-managed (`Gc<T>`) child of this value to `visit`.
    ///
    /// Distinct from [`Value::visit_gc_children`], which visits `&Value` for
    /// root enumeration: `gc_trace` yields the erased `Gc` node handles the
    /// cycle collector walks between managed nodes. Only variants already
    /// migrated to `Gc<T>` (§11 step 5+) yield a child; the rest are no-ops. A
    /// not-yet-migrated container that transitively holds a `Gc` node is a graph
    /// edge that bypasses the Gc graph until it too migrates — acceptable while
    /// the collector is off (design doc §3.1's wave phasing).
    pub(crate) fn gc_trace(&self, visit: &mut dyn FnMut(&ErasedGc)) {
        // Only migrated (`Gc<T>`) container variants yield a child; more arms
        // land as further types migrate (§11 5d: `ContainerRef`, ...).
        match self {
            Value::Hash(data) => visit(&data.erased()),
            Value::Array(data, _) => visit(&data.erased()),
            Value::Set(data, _) => visit(&data.erased()),
            Value::Bag(data, _) => visit(&data.erased()),
            Value::Mix(data, _) => visit(&data.erased()),
            Value::ContainerRef(cell) => visit(&cell.erased()),
            _ => {}
        }
    }
}

/// `ContainerRef` is a `Gc<Mutex<Value>>` scalar/element cell (§11 step 5e). Its
/// single GC child is the `Value` it holds, reached by locking. (`Trace` for a
/// std `Mutex` is allowed — `Trace` is a local trait — and only ever runs at a
/// collect safepoint where no program thread holds the lock, so it cannot
/// deadlock; the collector itself is still off until step 8.)
impl Trace for std::sync::Mutex<Value> {
    fn trace(&self, visit: &mut dyn FnMut(&ErasedGc)) {
        if let Ok(guard) = self.lock() {
            guard.gc_trace(visit);
        }
    }
}

/// The QuantHash datas hold `Value`s only in their `original_keys` back-map
/// (the primary `elements`/`counts`/`weights` maps are keyed by string and hold
/// no `Value`s), so tracing that map covers every GC child.
impl Trace for SetData {
    fn trace(&self, visit: &mut dyn FnMut(&ErasedGc)) {
        if let Some(keys) = &self.original_keys {
            for v in keys.values() {
                v.gc_trace(visit);
            }
        }
    }
}

impl Trace for BagData {
    fn trace(&self, visit: &mut dyn FnMut(&ErasedGc)) {
        if let Some(keys) = &self.original_keys {
            for v in keys.values() {
                v.gc_trace(visit);
            }
        }
    }
}

impl Trace for MixData {
    fn trace(&self, visit: &mut dyn FnMut(&ErasedGc)) {
        if let Some(keys) = &self.original_keys {
            for v in keys.values() {
                v.gc_trace(visit);
            }
        }
    }
}

impl Trace for ArrayData {
    fn trace(&self, visit: &mut dyn FnMut(&ErasedGc)) {
        for v in &self.items {
            v.gc_trace(visit);
        }
        if let Some(d) = &self.default {
            d.gc_trace(visit);
        }
    }
}

impl Trace for HashData {
    fn trace(&self, visit: &mut dyn FnMut(&ErasedGc)) {
        for v in self.map.values() {
            v.gc_trace(visit);
        }
        if let Some(keys) = &self.original_keys {
            for v in keys.values() {
                v.gc_trace(visit);
            }
        }
        if let Some(d) = &self.default {
            d.gc_trace(visit);
        }
    }
}

impl Value {
    #[allow(dead_code)]
    pub(crate) fn visit_gc_children(&self, visitor: &mut dyn RootVisitor) {
        match self {
            Value::Array(data, _) => data.visit_gc_children(visitor),
            Value::Hash(data) => data.visit_gc_children(visitor),
            Value::Set(data, _) => {
                if let Some(keys) = &data.original_keys {
                    visit_map_values(visitor, keys);
                }
            }
            Value::Bag(data, _) => {
                if let Some(keys) = &data.original_keys {
                    visit_map_values(visitor, keys);
                }
            }
            Value::Mix(data, _) => {
                if let Some(keys) = &data.original_keys {
                    visit_map_values(visitor, keys);
                }
            }
            Value::Pair(_, v) => visitor.visit_value(v),
            Value::ValuePair(k, v) => {
                visitor.visit_value(k);
                visitor.visit_value(v);
            }
            Value::Sub(data) => data.visit_gc_children(visitor),
            Value::Instance { attributes, .. } => attributes.visit_gc_children(visitor),
            Value::ContainerRef(cell) => {
                if let Ok(guard) = cell.lock() {
                    visitor.visit_value(&guard);
                }
            }
            Value::Promise(p) => p.visit_gc_children(visitor),
            Value::Channel(c) => c.visit_gc_children(visitor),
            _ => {}
        }
    }
}

impl ArrayData {
    #[allow(dead_code)]
    pub(crate) fn visit_gc_children(&self, visitor: &mut dyn RootVisitor) {
        for v in &self.items {
            visitor.visit_value(v);
        }
        if let Some(d) = &self.default {
            visitor.visit_value(d);
        }
    }
}

impl HashData {
    #[allow(dead_code)]
    pub(crate) fn visit_gc_children(&self, visitor: &mut dyn RootVisitor) {
        visit_map_values(visitor, &self.map);
        if let Some(keys) = &self.original_keys {
            visit_map_values(visitor, keys);
        }
        if let Some(d) = &self.default {
            visitor.visit_value(d);
        }
    }
}

impl SubData {
    #[allow(dead_code)]
    pub(crate) fn visit_gc_children(&self, visitor: &mut dyn RootVisitor) {
        self.env.visit_values(visitor);
        for v in &self.assumed_positional {
            visitor.visit_value(v);
        }
        visit_map_values(visitor, &self.assumed_named);
    }
}

impl InstanceAttrs {
    #[allow(dead_code)]
    pub(crate) fn visit_gc_children(&self, visitor: &mut dyn RootVisitor) {
        // Go through the existing read-guard accessor rather than reaching
        // into the private cell directly — respects the encapsulation
        // boundary and same-thread read/write-deferral discipline
        // `AttrReadGuard` already encodes (see the type's doc comment).
        for v in self.as_map().values() {
            visitor.visit_value(v);
        }
    }
}

impl SharedPromise {
    #[allow(dead_code)]
    pub(crate) fn visit_gc_children(&self, visitor: &mut dyn RootVisitor) {
        if let Ok(state) = self.inner.0.lock() {
            visitor.visit_value(&state.result);
        }
        // `waiters` are boxed `FnOnce` closures — opaque to a visitor (design
        // doc §2.2's PromiseState note). Any `Value` a callback captures is
        // unreachable through this node until the promise resolves and the
        // callback actually runs.
    }
}

impl SharedChannel {
    #[allow(dead_code)]
    pub(crate) fn visit_gc_children(&self, visitor: &mut dyn RootVisitor) {
        if let Ok(state) = self.inner.0.lock() {
            for v in &state.queue {
                visitor.visit_value(v);
            }
            if let Some(f) = &state.failure {
                visitor.visit_value(f);
            }
            state.closed_promise.visit_gc_children(visitor);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    struct CountingVisitor {
        count: usize,
    }

    impl RootVisitor for CountingVisitor {
        fn visit_value(&mut self, _value: &Value) {
            self.count += 1;
        }
    }

    #[test]
    fn array_children_include_items_and_default() {
        let mut data = ArrayData {
            items: vec![Value::Int(1), Value::Int(2)],
            ..Default::default()
        };
        data.default = Some(Box::new(Value::Int(0)));
        let value = Value::Array(crate::gc::Gc::new(data), crate::value::ArrayKind::Array);

        let mut visitor = CountingVisitor { count: 0 };
        value.visit_gc_children(&mut visitor);
        assert_eq!(visitor.count, 3);
    }

    #[test]
    fn hash_children_include_map_values() {
        let mut map = std::collections::HashMap::new();
        map.insert("a".to_string(), Value::Int(1));
        map.insert("b".to_string(), Value::Int(2));
        let data = HashData {
            map,
            ..Default::default()
        };
        let value = Value::Hash(crate::gc::Gc::new(data));

        let mut visitor = CountingVisitor { count: 0 };
        value.visit_gc_children(&mut visitor);
        assert_eq!(visitor.count, 2);
    }

    #[test]
    fn scalar_values_have_no_gc_children() {
        let mut visitor = CountingVisitor { count: 0 };
        Value::Int(42).visit_gc_children(&mut visitor);
        Value::Str(Arc::new("hi".to_string())).visit_gc_children(&mut visitor);
        Value::Nil.visit_gc_children(&mut visitor);
        assert_eq!(visitor.count, 0);
    }

    #[test]
    fn container_ref_visits_its_locked_value() {
        let cell = Value::ContainerRef(crate::gc::Gc::new(std::sync::Mutex::new(Value::Int(7))));
        let mut visitor = CountingVisitor { count: 0 };
        cell.visit_gc_children(&mut visitor);
        assert_eq!(visitor.count, 1);
    }

    #[test]
    fn promise_visits_its_result() {
        let promise = SharedPromise::new();
        promise.keep(Value::Int(9), String::new(), String::new());
        let mut visitor = CountingVisitor { count: 0 };
        Value::Promise(promise).visit_gc_children(&mut visitor);
        assert_eq!(visitor.count, 1);
    }
}
