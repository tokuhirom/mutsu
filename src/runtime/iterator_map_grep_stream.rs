//! The built-in `Iterator` over a deferred `.map`/`.grep` Seq (#10186).
//!
//! `(1..5).map(&f).iterator.pull-one` runs `&f` once, as Rakudo's map
//! iterator does. `.iterator` steals the Seq's [`SeqSource::MapGrep`]
//! (`SeqBody::take_map_grep_stream_source`) and keeps it on the instance as a
//! private stream body under [`MAP_GREP_STREAM_ATTR`]; every protocol call
//! pulls from it exactly as many elements as it needs.
//!
//! The instance's `items` attribute is a *window*, not the whole produced
//! prefix: the elements pulled but not yet handed out. A top-up drops the
//! consumed part of the window before appending, and a fully consumed window
//! is emptied, so neither the window nor the per-call copy of it grows with
//! the number of elements already delivered — draining with `pull-one` is
//! O(n) overall.
//!
//! Both receiver shapes (a variable, `methods_mut_dispatch.rs`, and a
//! temporary or array element, `methods_call_dispatch.rs`) commit through the
//! instance's shared attribute cell, so every alias sees the advance.
//!
//! Spec: <https://docs.raku.org/type/Iterator>

use crate::symbol::Symbol;
use crate::value::{InstanceAttrs, RuntimeError, SeqSource, Value, ValueView};
use std::collections::HashMap;

/// The instance attribute holding the stream body.
pub(crate) const MAP_GREP_STREAM_ATTR: &str = "map_grep_stream";

/// Build the `Iterator` instance over a stolen `.map`/`.grep` source.
/// `prefix` is what a prefix pull already produced; `lazy` is the Seq's
/// `.lazy` mark.
// Cost: O(1), plus moving `prefix`.
pub(crate) fn map_grep_stream_iterator(prefix: Vec<Value>, source: SeqSource, lazy: bool) -> Value {
    let mut attrs = HashMap::new();
    attrs.insert("items".to_string(), Value::array(prefix));
    attrs.insert("index".to_string(), Value::int(0));
    attrs.insert(
        MAP_GREP_STREAM_ATTR.to_string(),
        Value::seq_deferred(source),
    );
    if lazy {
        attrs.insert("is_lazy".to_string(), Value::TRUE);
    }
    Value::make_instance(Symbol::intern("Iterator"), attrs)
}

/// The window and cursor read off an instance, plus its stream.
struct StreamState {
    stream: Value,
    items: Vec<Value>,
    index: usize,
    lazy: bool,
}

// Cost: O(w), w = elements in the window.
fn read_state(attributes: &InstanceAttrs) -> Option<StreamState> {
    let map = attributes.as_map();
    let stream = map.get(MAP_GREP_STREAM_ATTR)?.clone();
    let items = match map.get("items").map(Value::view) {
        Some(ValueView::Array(values, ..)) => values.to_vec(),
        _ => Vec::new(),
    };
    let index = match map.get("index").map(Value::view) {
        Some(ValueView::Int(i)) if i >= 0 => (i as usize).min(items.len()),
        _ => 0,
    };
    let lazy = map.contains_key("is_lazy");
    Some(StreamState {
        stream,
        items,
        index,
        lazy,
    })
}

impl crate::Interpreter {
    /// Run an Iterator-protocol call on a stream-backed built-in `Iterator`.
    /// `None` when `attributes` is not one, or for a method outside the
    /// protocol family (`can`, `is-lazy`, ... go through ordinary dispatch).
    // Cost: one callback call per source element the call consumes, plus
    // O(w), w = elements in the window.
    pub(crate) fn map_grep_stream_protocol_call(
        &mut self,
        attributes: &InstanceAttrs,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(
            method,
            "pull-one"
                | "push-exactly"
                | "push-at-least"
                | "push-all"
                | "push-until-lazy"
                | "sink-all"
                | "skip-one"
                | "skip-at-least"
                | "skip-at-least-pull-one"
                | "count-only"
                | "bool-only"
        ) {
            return None;
        }
        let state = read_state(attributes)?;
        Some(self.map_grep_stream_call(attributes, state, method, args))
    }

    fn map_grep_stream_call(
        &mut self,
        attributes: &InstanceAttrs,
        state: StreamState,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let need = match method {
            // Rakudo's map iterator is not a PredictiveIterator; answering
            // from what the source still produces is the honest count.
            "count-only" => None,
            "bool-only" => Some(state.index + 1),
            // Over a finite source `push-until-lazy` is `push-all`.
            "push-until-lazy" if !state.lazy => None,
            _ => super::iterator_protocol::needed_len(method, state.index, args),
        };
        let StreamState {
            stream,
            mut items,
            mut index,
            ..
        } = state;
        let mut window_changed =
            self.map_grep_stream_fill(&stream, &mut items, &mut index, need)?;
        let ret = match method {
            "count-only" => Value::int((items.len() - index) as i64),
            "bool-only" => Value::truth(index < items.len()),
            _ => {
                let step = super::iterator_protocol::step(method, &items, index, args)
                    .expect("every other method in the family is a stepping method");
                if let Some(range) = step.append {
                    let vals = items[range].to_vec();
                    self.iterator_append_to_array_arg(args, &vals);
                }
                index = step.new_index;
                step.ret
            }
        };
        // A fully consumed window holds nothing anyone can read again.
        if index >= items.len() && !items.is_empty() {
            items.clear();
            index = 0;
            window_changed = true;
        }
        let mut ops = vec![(Symbol::intern("index"), Some(Value::int(index as i64)))];
        if window_changed {
            ops.push((Symbol::intern("items"), Some(Value::array(items))));
        }
        attributes.write_keys(ops);
        Ok(ret)
    }

    /// Make the window hold at least `need - index` unconsumed elements
    /// (`need` counts from the window's start; `None` drains the source),
    /// dropping the consumed part of the window first. Returns whether the
    /// window changed.
    // Cost: one callback call per source element pulled, plus O(w),
    // w = unconsumed elements in the window.
    fn map_grep_stream_fill(
        &mut self,
        stream: &Value,
        items: &mut Vec<Value>,
        index: &mut usize,
        need: Option<usize>,
    ) -> Result<bool, RuntimeError> {
        let missing = match need {
            Some(n) if n <= items.len() => return Ok(false),
            Some(n) => Some(n - items.len()),
            None => None,
        };
        let ValueView::Seq(body) = stream.view() else {
            return Ok(false);
        };
        let pulled = body.advance_map_grep_source(|source| match missing {
            Some(n) => self.pull_map_grep_prefix(source, n),
            None => self.pull_map_grep_remaining(source),
        })?;
        let Some(pulled) = pulled else {
            return Ok(false);
        };
        items.drain(..*index);
        *index = 0;
        items.extend(pulled);
        Ok(true)
    }

    /// Every element a stream-backed built-in `Iterator` has left, draining
    /// its source and leaving it exhausted — for the readers that take a
    /// built-in iterator's remaining elements wholesale (`List.new($iter)`,
    /// a `*@` binding of an iterator). `None` when `attributes` is not one.
    // Cost: one callback call per source element not yet pulled.
    pub(crate) fn map_grep_stream_drain(
        &mut self,
        attributes: &InstanceAttrs,
    ) -> Option<Result<Vec<Value>, RuntimeError>> {
        let state = read_state(attributes)?;
        Some(self.map_grep_stream_drain_state(attributes, state))
    }

    fn map_grep_stream_drain_state(
        &mut self,
        attributes: &InstanceAttrs,
        state: StreamState,
    ) -> Result<Vec<Value>, RuntimeError> {
        let StreamState {
            stream,
            mut items,
            mut index,
            ..
        } = state;
        self.map_grep_stream_fill(&stream, &mut items, &mut index, None)?;
        let rest = items.split_off(index);
        attributes.write_keys(vec![
            (Symbol::intern("index"), Some(Value::int(0))),
            (Symbol::intern("items"), Some(Value::array(Vec::new()))),
        ]);
        Ok(rest)
    }
}
