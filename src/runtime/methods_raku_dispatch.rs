//! `.raku`/`.perl` on a container that holds a value whose repr is only
//! reachable through method dispatch.
//!
//! The pure renderer `builtins::methods_0arg::raku_repr::raku_value` owns every
//! container rule (bracket style, itemization, trailing commas, adverbial pair
//! keys, cycle detection). It has no interpreter, though, so an `Instance` leaf
//! falls through to `to_string_value()` — `[Foo.new(x => 1)].raku` rendered as
//! `[Foo()]`, and a built-in object type likewise (`(1.Supply,).raku` →
//! `(Supply(),)`).
//!
//! Rather than duplicate the container rules in an interpreter-side walk (the
//! shape `.gist` uses), this module keeps the single renderer and only rewrites
//! the *leaves*: each dispatch-needing element gets its `.raku` called through
//! the interpreter, and the resulting text is spliced back into a copy of the
//! tree as a `raku_raw` marker that `raku_value` emits verbatim.

use super::Interpreter;
use crate::builtins::methods_0arg::raku_repr::{needs_raku_dispatch, raku_raw};
use crate::value::{ArrayData, HashData, Value, ValueView};

/// Depth cap for the walk. Cycles are caught by container identity (below);
/// this only keeps a pathologically deep structure from blowing the stack.
const MAX_DEPTH: usize = 256;

/// Container identity, for the visited set. Only the `Gc`-backed containers can
/// participate in a cycle, so the others have no identity to track.
fn container_id(value: &Value) -> Option<usize> {
    match value.view() {
        ValueView::Array(data, _) => Some(crate::gc::Gc::as_ptr(&data) as usize),
        ValueView::Hash(data) => Some(crate::gc::Gc::as_ptr(&data) as usize),
        _ => None,
    }
}

fn typed_keys_contain(
    original_keys: &Option<std::collections::HashMap<String, Value>>,
    seen: &mut std::collections::HashSet<usize>,
    depth: usize,
) -> bool {
    original_keys.as_ref().is_some_and(|keys| {
        keys.values()
            .any(|k| contains_dispatch_leaf_seen(k, seen, depth + 1))
    })
}

/// Whether any leaf inside `value` needs method dispatch to render.
fn contains_dispatch_leaf(value: &Value) -> bool {
    let mut seen = std::collections::HashSet::new();
    contains_dispatch_leaf_seen(value, &mut seen, 0)
}

/// `seen` holds every container already walked — not just the ancestors. A
/// circular structure (`@a = 42, @a`) would otherwise be re-walked once per
/// path reaching it, which is exponential for a graph with two cyclic edges
/// (`%h = :b(%h), :c(@b); @b = %h, @b, 42` — `roast/S32-array/perl.t`).
fn contains_dispatch_leaf_seen(
    value: &Value,
    seen: &mut std::collections::HashSet<usize>,
    depth: usize,
) -> bool {
    if depth > MAX_DEPTH {
        return false;
    }
    if needs_raku_dispatch(value) {
        return true;
    }
    if let Some(id) = container_id(value)
        && !seen.insert(id)
    {
        return false;
    }
    match value.view() {
        ValueView::Scalar(inner) => contains_dispatch_leaf_seen(inner, seen, depth + 1),
        ValueView::ContainerRef(cell) => {
            let inner = cell.lock().unwrap().clone();
            contains_dispatch_leaf_seen(&inner, seen, depth + 1)
        }
        ValueView::Hash(map) => {
            let values: Vec<Value> = map.map.values().cloned().collect();
            values
                .iter()
                .any(|v| contains_dispatch_leaf_seen(v, seen, depth + 1))
        }
        // A Set/Bag/Mix keeps its element *objects* in `original_keys`; those
        // are what `.raku` renders (the string keys are the internal index).
        ValueView::Set(data, _) => typed_keys_contain(&data.original_keys, seen, depth),
        ValueView::Bag(data, _) => typed_keys_contain(&data.original_keys, seen, depth),
        ValueView::Mix(data, _) => typed_keys_contain(&data.original_keys, seen, depth),
        ValueView::Capture { positional, named } => {
            positional
                .iter()
                .any(|v| contains_dispatch_leaf_seen(v, seen, depth + 1))
                || named
                    .values()
                    .any(|v| contains_dispatch_leaf_seen(v, seen, depth + 1))
        }
        ValueView::Pair(_, v) => contains_dispatch_leaf_seen(v, seen, depth + 1),
        ValueView::ValuePair(k, v) => {
            contains_dispatch_leaf_seen(k, seen, depth + 1)
                || contains_dispatch_leaf_seen(v, seen, depth + 1)
        }
        _ => match value.as_list_items() {
            Some(items) => {
                let items = items.to_vec();
                items
                    .iter()
                    .any(|v| contains_dispatch_leaf_seen(v, seen, depth + 1))
            }
            None => false,
        },
    }
}

/// Whether `value` is a *container* holding a leaf whose `.raku` needs method
/// dispatch — i.e. whether the pure native fast path must be bypassed so
/// [`Interpreter::raku_repr_with_dispatch`] can render it. A bare leaf is
/// excluded: it dispatches normally (routing it here would recurse).
pub(crate) fn container_needs_raku_dispatch(value: &Value) -> bool {
    matches!(
        value.view(),
        ValueView::Array(..)
            | ValueView::Seq(..)
            | ValueView::HyperSeq(..)
            | ValueView::RaceSeq(..)
            | ValueView::Slip(..)
            | ValueView::Hash(..)
            | ValueView::Pair(..)
            | ValueView::ValuePair(..)
            | ValueView::Scalar(..)
            | ValueView::Set(..)
            | ValueView::Bag(..)
            | ValueView::Mix(..)
            | ValueView::Capture { .. }
    ) && contains_dispatch_leaf(value)
}

impl Interpreter {
    /// Rebuild `value` with every dispatch-needing leaf replaced by its
    /// interpreter-rendered `.raku` text. Containers are rebuilt in place with
    /// their kind/metadata preserved so the pure renderer still sees the same
    /// shape.
    fn expand_raku_leaves(
        &mut self,
        value: &Value,
        active: &mut Vec<usize>,
        depth: usize,
    ) -> Value {
        // A subtree with nothing to dispatch is shared as-is.
        if depth > MAX_DEPTH || !contains_dispatch_leaf(value) {
            return value.clone();
        }
        // A container that is its own ancestor is left for `raku_value`'s cycle
        // placeholder; rebuilding it would recurse forever.
        if let Some(id) = container_id(value) {
            if active.contains(&id) {
                return value.clone();
            }
            active.push(id);
            let rebuilt = self.expand_container(value, active, depth);
            active.pop();
            return rebuilt;
        }
        self.expand_container(value, active, depth)
    }

    fn expand_container(&mut self, value: &Value, active: &mut Vec<usize>, depth: usize) -> Value {
        if needs_raku_dispatch(value) {
            // A self-referencing object reaches itself through its own
            // attributes; dispatching again would recurse forever, so a
            // re-entered instance falls back to the pure repr.
            let id = match value.view() {
                ValueView::Instance { id, .. } => Some(id),
                _ => None,
            };
            if let Some(id) = id {
                if self.raku_leaf_active.contains(&id) {
                    return value.clone();
                }
                self.raku_leaf_active.push(id);
            }
            let rendered = self.call_method_with_values(value.clone(), "raku", vec![]);
            if let Some(id) = id
                && let Some(pos) = self.raku_leaf_active.iter().rposition(|x| *x == id)
            {
                self.raku_leaf_active.remove(pos);
            }
            return match rendered {
                Ok(rendered) => raku_raw(rendered.to_string_value()),
                Err(_) => value.clone(),
            };
        }
        match value.view() {
            ValueView::Array(data, kind) => {
                let mut rebuilt = ArrayData::clone(&data);
                let items = std::mem::take(&mut rebuilt.items);
                rebuilt.items = items
                    .iter()
                    .map(|item| self.expand_raku_leaves(item, active, depth + 1))
                    .collect();
                Value::array_with_kind(crate::gc::Gc::new(rebuilt), kind)
            }
            ValueView::Hash(map) => {
                let entries: Vec<(String, Value)> = map
                    .map
                    .iter()
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect();
                let mut rebuilt = HashData::clone(&map);
                for (k, v) in entries {
                    let expanded = self.expand_raku_leaves(&v, active, depth + 1);
                    rebuilt.map.insert(k, expanded);
                }
                let itemized = value.hash_is_itemized();
                Value::hash(rebuilt).with_hash_itemized(itemized)
            }
            ValueView::Pair(key, v) => {
                let key = key.clone();
                let v = v.clone();
                Value::pair(key, self.expand_raku_leaves(&v, active, depth + 1))
            }
            ValueView::ValuePair(k, v) => {
                let (k, v) = (k.clone(), v.clone());
                let k = self.expand_raku_leaves(&k, active, depth + 1);
                let v = self.expand_raku_leaves(&v, active, depth + 1);
                Value::value_pair(k, v)
            }
            ValueView::Scalar(inner) => {
                let inner = inner.clone();
                Value::scalar(self.expand_raku_leaves(&inner, active, depth + 1))
            }
            ValueView::ContainerRef(cell) => {
                let inner = cell.lock().unwrap().clone();
                self.expand_raku_leaves(&inner, active, depth + 1)
            }
            ValueView::Seq(_) | ValueView::HyperSeq(_) | ValueView::RaceSeq(_) => {
                let items = self.expand_list_items(value, active, depth);
                match value.view() {
                    ValueView::HyperSeq(_) => Value::hyper_seq_arc(std::sync::Arc::new(items)),
                    ValueView::RaceSeq(_) => Value::race_seq_arc(std::sync::Arc::new(items)),
                    _ => Value::seq_arc(std::sync::Arc::new(items)),
                }
            }
            ValueView::Capture { positional, named } => {
                let (positional, named) = (positional.clone(), named.clone());
                let positional: Vec<Value> = positional
                    .iter()
                    .map(|v| self.expand_raku_leaves(v, active, depth + 1))
                    .collect();
                let named = named
                    .into_iter()
                    .map(|(k, v)| {
                        let expanded = self.expand_raku_leaves(&v, active, depth + 1);
                        (k, expanded)
                    })
                    .collect();
                Value::capture(positional, named)
            }
            ValueView::Set(data, is_mut) => {
                let mut rebuilt = crate::value::SetData::clone(&data);
                rebuilt.original_keys =
                    self.expand_typed_keys(rebuilt.original_keys, active, depth);
                Value::set_parts(crate::gc::Gc::new(rebuilt), is_mut)
            }
            ValueView::Bag(data, is_mut) => {
                let mut rebuilt = crate::value::BagData::clone(&data);
                rebuilt.original_keys =
                    self.expand_typed_keys(rebuilt.original_keys, active, depth);
                Value::bag_parts(crate::gc::Gc::new(rebuilt), is_mut)
            }
            ValueView::Mix(data, is_mut) => {
                let mut rebuilt = crate::value::MixData::clone(&data);
                rebuilt.original_keys =
                    self.expand_typed_keys(rebuilt.original_keys, active, depth);
                Value::mix_parts(crate::gc::Gc::new(rebuilt), is_mut)
            }
            ValueView::Slip(_) => {
                let items = self.expand_list_items(value, active, depth);
                Value::slip_arc(std::sync::Arc::new(items))
            }
            _ => value.clone(),
        }
    }

    fn expand_typed_keys(
        &mut self,
        original_keys: Option<std::collections::HashMap<String, Value>>,
        active: &mut Vec<usize>,
        depth: usize,
    ) -> Option<std::collections::HashMap<String, Value>> {
        original_keys.map(|keys| {
            keys.into_iter()
                .map(|(k, v)| {
                    let expanded = self.expand_raku_leaves(&v, active, depth + 1);
                    (k, expanded)
                })
                .collect()
        })
    }

    fn expand_list_items(
        &mut self,
        value: &Value,
        active: &mut Vec<usize>,
        depth: usize,
    ) -> Vec<Value> {
        let items: Vec<Value> = value
            .as_list_items()
            .map(|items| items.to_vec())
            .unwrap_or_default();
        items
            .iter()
            .map(|item| self.expand_raku_leaves(item, active, depth + 1))
            .collect()
    }

    /// The `.raku`/`.perl` text of a container holding a dispatch-needing leaf,
    /// or `None` when the pure renderer already handles the value on its own.
    pub(crate) fn raku_repr_with_dispatch(&mut self, target: &Value) -> Option<String> {
        if !container_needs_raku_dispatch(target) {
            return None;
        }
        let expanded = self.expand_raku_leaves(target, &mut Vec::new(), 0);
        Some(crate::builtins::methods_0arg::raku_repr::raku_value(
            &expanded,
        ))
    }

    /// Render one element for `.raku` with leaf dispatch applied (used by the
    /// typed-container `.raku` paths, which build their own outer text).
    pub(crate) fn raku_element_repr(&mut self, value: &Value) -> String {
        let expanded = self.expand_raku_leaves(value, &mut Vec::new(), 0);
        crate::builtins::methods_0arg::raku_repr::raku_value(&expanded)
    }
}
