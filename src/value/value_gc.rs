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

use std::sync::{Condvar, Mutex};

use crate::gc::{ErasedGc, RootVisitor, Trace, visit_map_values};

use super::{
    ArrayData, BagData, ChannelState, HashData, InstanceAttrs, LazyList, MixData, PromiseState,
    SetData, SharedChannel, SharedPromise, SubData, Value,
};

impl Value {
    /// Hand each direct GC-managed (`Gc<T>`) child of this value to `visit`.
    ///
    /// Distinct from [`Value::visit_gc_children`], which visits `&Value` for
    /// root enumeration: `gc_trace` yields the erased `Gc` node handles the
    /// cycle collector walks between managed nodes. Migrated `Gc<T>` variants
    /// yield the node; non-node wrappers that own/share `Value`s recurse so a
    /// `Gc` node nested inside them is still reached (otherwise the wrapper is an
    /// invisible edge and a cycle through it is missed — an under-collect).
    pub(crate) fn gc_trace(&self, visit: &mut dyn FnMut(&ErasedGc)) {
        match self {
            // Migrated `Gc<T>` node variants: yield the node itself. The
            // collector traces the node's own children via its `Trace` impl, so
            // we do NOT recurse into its contents here.
            Value::Hash(data) => visit(&data.erased()),
            Value::Array(data, _) => visit(&data.erased()),
            Value::Set(data, _) => visit(&data.erased()),
            Value::Bag(data, _) => visit(&data.erased()),
            Value::Mix(data, _) => visit(&data.erased()),
            Value::ContainerRef(cell) => visit(&cell.erased()),
            Value::Sub(data) => visit(&data.erased()),
            Value::Instance { attributes, .. } => visit(&attributes.erased()),
            Value::LazyList(ll) => visit(&ll.erased()),

            // Non-node wrappers that own/share `Value`s: recurse. They cannot
            // themselves form a cycle without passing through a `Gc` node above
            // (`Box` is uniquely owned; the `Arc<Vec>` sequences are immutable),
            // so the recursion terminates. `WeakSub` is a WEAK edge — not traced.
            Value::Pair(_, v) | Value::Scalar(v) => v.gc_trace(visit),
            Value::ValuePair(k, v) => {
                k.gc_trace(visit);
                v.gc_trace(visit);
            }
            Value::Capture { positional, named } => {
                for v in positional.iter() {
                    v.gc_trace(visit);
                }
                for v in named.values() {
                    v.gc_trace(visit);
                }
            }
            Value::Seq(items)
            | Value::HyperSeq(items)
            | Value::RaceSeq(items)
            | Value::Slip(items) => {
                for v in items.iter() {
                    v.gc_trace(visit);
                }
            }
            Value::Mixin(inner, overrides) => {
                inner.gc_trace(visit);
                for v in overrides.values() {
                    v.gc_trace(visit);
                }
            }
            Value::Junction { values, .. } => {
                for v in values.iter() {
                    v.gc_trace(visit);
                }
            }
            Value::GenericRange { start, end, .. } => {
                start.gc_trace(visit);
                end.gc_trace(visit);
            }
            Value::Proxy {
                fetcher, storer, ..
            } => {
                fetcher.gc_trace(visit);
                storer.gc_trace(visit);
            }
            // Migrated async `Gc<T>` node variants (§11 steps 6-7): yield the
            // node; its `Trace` impl walks the held result / queue / callbacks.
            Value::Promise(p) => visit(&p.erased()),
            Value::Channel(c) => visit(&c.erased()),
            _ => {}
        }
    }
}

/// A promise node's single `Value` edge is its resolved `result`. Its queued
/// `waiters` are boxed `FnOnce` closures — opaque to a visitor (they may capture
/// `Value`s, but those are unreachable through this node until the callback runs
/// and are dropped when the promise resolves), so they are not traced.
/// Async node migration (§11 step 6).
impl Trace for (Mutex<PromiseState>, Condvar) {
    fn trace(&self, visit: &mut dyn FnMut(&ErasedGc)) {
        if let Ok(state) = self.0.lock() {
            state.result.gc_trace(visit);
        }
    }
    fn drop_gc_edges(&mut self) {
        if let Ok(state) = self.0.get_mut() {
            state.result = Value::Nil;
            state.waiters.clear();
        }
    }
}

/// A channel node's `Value` edges are its buffered `queue`, its `failure`, and
/// its `closed_promise` node. Async node migration (§11 step 7).
impl Trace for (Mutex<ChannelState>, Condvar) {
    fn trace(&self, visit: &mut dyn FnMut(&ErasedGc)) {
        if let Ok(state) = self.0.lock() {
            for v in &state.queue {
                v.gc_trace(visit);
            }
            if let Some(f) = &state.failure {
                f.gc_trace(visit);
            }
            visit(&state.closed_promise.erased());
        }
    }
    fn drop_gc_edges(&mut self) {
        if let Ok(state) = self.0.get_mut() {
            state.queue.clear();
            state.failure = None;
        }
    }
}

/// A lazy list captures an `env` and a materialization `cache` of `Value`s, both
/// of which can close a cycle (a gather/closure sequence that captures the very
/// list it produces). Third-wave migration (§11 step 10).
///
/// First cut traces the `env` + `cache` + `elems_count` — the dominant
/// env-capture cycle sources. The on-demand spec states (coroutine / lazy-pipe /
/// closure-seq / scan / walk / cat) also hold `Value`s; leaving them untraced is
/// *conservative* (a cycle routed only through them is not reclaimed, never
/// wrongly freed) and is the documented step-10 follow-up.
impl Trace for LazyList {
    fn trace(&self, visit: &mut dyn FnMut(&ErasedGc)) {
        for v in self.env.values() {
            v.gc_trace(visit);
        }
        if let Ok(cache) = self.cache.lock()
            && let Some(items) = cache.as_ref()
        {
            for v in items {
                v.gc_trace(visit);
            }
        }
        if let Some(n) = &self.elems_count {
            n.gc_trace(visit);
        }
    }
    fn drop_gc_edges(&mut self) {
        for v in self.env.values_mut() {
            *v = Value::Nil;
        }
        if let Ok(mut cache) = self.cache.lock() {
            *cache = None;
        }
        self.elems_count = None;
    }
}

/// A closure's captured `env` and `.assume`d args hold `Value`s that can close
/// a cycle (a recursive closure captures itself; two closures capture each
/// other). Second-wave migration (§11 step 9).
impl Trace for SubData {
    fn trace(&self, visit: &mut dyn FnMut(&ErasedGc)) {
        for v in self.env.values() {
            v.gc_trace(visit);
        }
        for v in &self.assumed_positional {
            v.gc_trace(visit);
        }
        for v in self.assumed_named.values() {
            v.gc_trace(visit);
        }
    }
    fn drop_gc_edges(&mut self) {
        for v in self.env.values_mut() {
            *v = Value::Nil;
        }
        self.assumed_positional.clear();
        self.assumed_named.clear();
    }
}

/// An object's per-attribute cell holds `Value`s that can close a cycle
/// (`$obj.parent = $obj`, mutually-referential objects). The attribute cell is
/// interior-mutable (`AttrCell = Arc<RwLock<..>>`), so severing is a plain cell
/// write. Second-wave migration (§11 step 9).
impl Trace for InstanceAttrs {
    fn trace(&self, visit: &mut dyn FnMut(&ErasedGc)) {
        for v in self.as_map().values() {
            v.gc_trace(visit);
        }
    }
    fn drop_gc_edges(&mut self) {
        self.clear_gc_edges();
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

    fn drop_gc_edges(&mut self) {
        // The `original_keys` back-map is the QuantHash's only Value-holding field.
        self.original_keys = None;
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

    fn drop_gc_edges(&mut self) {
        self.original_keys = None;
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

    fn drop_gc_edges(&mut self) {
        self.original_keys = None;
    }
}

impl Trace for Mutex<Value> {
    fn trace(&self, visit: &mut dyn FnMut(&ErasedGc)) {
        // The bind cell's inner value is the single edge out of a `ContainerRef`
        // node — this is what makes self-binding cycles (`%h<k> := %h`,
        // `@a[0] := @a`) reachable through the Gc graph.
        if let Ok(inner) = self.lock() {
            inner.gc_trace(visit);
        }
    }

    fn drop_gc_edges(&mut self) {
        // Break the cell's single edge (reclaim). `&mut self` => no lock needed.
        if let Ok(inner) = self.get_mut() {
            *inner = Value::Nil;
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

    fn drop_gc_edges(&mut self) {
        // Clearing the elements drops every outgoing edge. `&mut self` is
        // supplied by the collector via the backing `Arc` (gc::gc_drop_edges).
        self.items.clear();
        self.default = None;
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

    fn drop_gc_edges(&mut self) {
        self.map.clear();
        self.original_keys = None;
        self.default = None;
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
    /// The type-erased GC node handle for this promise (its `Gc` inner), for the
    /// collector's `Value::Promise` trace edge.
    pub(crate) fn erased(&self) -> ErasedGc {
        self.inner.erased()
    }

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
    /// The type-erased GC node handle for this channel (its `Gc` inner), for the
    /// collector's `Value::Channel` trace edge.
    pub(crate) fn erased(&self) -> ErasedGc {
        self.inner.erased()
    }

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
