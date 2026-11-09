//! The one implementation of a built-in `Iterator` instance's protocol methods,
//! the handler behind every row of `method_table/instances/iterator.rs`
//! (ADR-11276 §9.126).
//!
//! A built-in iterator is one of three kinds, told apart by its attributes:
//! a deferred `.map`/`.grep` stream (`iterator_map_grep_stream.rs`), a `squish`
//! scan (`squish_source`, pulled one kept element at a time, calling the
//! `:as`/`:with` callbacks), or an array-backed cursor (`items`/`index`,
//! topped up from a `lazy_source` as far as the call needs). Each commits its
//! advanced state through the instance's shared attribute cell, so every alias
//! of the iterator sees the step, whatever the receiver was.
//!
//! Spec: <https://docs.raku.org/type/Iterator>

use super::iterator_protocol::{IterOp, step};
use crate::symbol::Symbol;
use crate::value::{InstanceAttrs, RuntimeError, Value, ValueView};

/// The state of a `squish` scan, read off the instance and written back after
/// the call.
struct SquishScan {
    source: Vec<Value>,
    scan_index: usize,
    prev_key: Value,
    initialized: bool,
    as_func: Option<Value>,
    with_func: Option<Value>,
}

impl SquishScan {
    // Cost: O(n), n = source elements (copied).
    fn read(attributes: &InstanceAttrs) -> Option<Self> {
        let map = attributes.as_map();
        let Some(ValueView::Array(source, ..)) = map.get("squish_source").map(Value::view) else {
            return None;
        };
        let callback = |key: &str| map.get(key).cloned().filter(|v| !v.is_nil());
        Some(SquishScan {
            source: source.to_vec(),
            scan_index: match map.get("squish_scan_index").map(Value::view) {
                Some(ValueView::Int(i)) if i >= 0 => i as usize,
                _ => 0,
            },
            prev_key: map.get("squish_prev_key").cloned().unwrap_or(Value::NIL),
            initialized: matches!(
                map.get("squish_initialized").map(Value::view),
                Some(ValueView::Bool(true))
            ),
            as_func: callback("squish_as"),
            with_func: callback("squish_with"),
        })
    }

    // Cost: O(1).
    fn write(&self, attributes: &InstanceAttrs) {
        attributes.write_keys(vec![
            (
                Symbol::intern("squish_scan_index"),
                Some(Value::int(self.scan_index as i64)),
            ),
            (
                Symbol::intern("squish_prev_key"),
                Some(self.prev_key.clone()),
            ),
            (
                Symbol::intern("squish_initialized"),
                Some(Value::truth(self.initialized)),
            ),
        ]);
    }
}

impl crate::Interpreter {
    /// Run one protocol method on a built-in `Iterator` instance. `None` when
    /// `target` is not one.
    // Cost: O(1) per element the call consumes, plus a callback call per
    // element for a stream or a `squish` scan with callbacks, plus forcing a
    // lazy source as far as the call needs.
    pub(crate) fn builtin_iterator_call(
        &mut self,
        target: &Value,
        op: IterOp,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let ValueView::Instance {
            attributes,
            class_name,
            ..
        } = target.view()
        else {
            return None;
        };
        if class_name == "FromLoopIterator" {
            return Some(self.step_loop_iterator(target, op));
        }
        if let Some(result) = self.map_grep_stream_protocol_call(&attributes, op, args) {
            return Some(result);
        }
        if let Some(mut scan) = SquishScan::read(&attributes) {
            let result = self.squish_iterator_call(&mut scan, op, args);
            scan.write(&attributes);
            return Some(result);
        }
        Some(self.array_iterator_call(&attributes, op, args))
    }

    /// A protocol call on the iterator of `Seq.from-loop(&body)`: each pull runs
    /// the body once (a body answering `IterationEnd`, or a `last`, ends it).
    /// Only the stepping methods are implemented; the `push-*` family needs a
    /// target array and is left to the rows' generic fallbacks.
    // Cost: one body call per element pulled.
    fn step_loop_iterator(
        &mut self,
        target: &Value,
        op: IterOp,
    ) -> Result<Value, RuntimeError> {
        match op {
            IterOp::PullOne => {
                let mut pulled = self.pull_from_loop_iterator_to_vec(target, 1)?;
                Ok(if pulled.is_empty() {
                    Value::iteration_end()
                } else {
                    pulled.remove(0)
                })
            }
            IterOp::SkipOne => {
                let pulled = self.pull_from_loop_iterator_to_vec(target, 1)?;
                Ok(Value::int(i64::from(!pulled.is_empty())))
            }
            IterOp::SinkAll => {
                self.pull_from_loop_iterator_to_vec(target, usize::MAX)?;
                Ok(Value::iteration_end())
            }
            _ => Err(RuntimeError::new(
                "this Seq.from-loop iterator supports pull-one, skip-one and sink-all only",
            )),
        }
    }

    /// A protocol call on an array-backed iterator.
    // Cost: O(1), plus O(k) for an appended range of k elements and O(n) to
    // copy the prefix when a lazy source is topped up (#16475: the items are
    // no longer copied on the ordinary call).
    fn array_iterator_call(
        &mut self,
        attributes: &InstanceAttrs,
        op: IterOp,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if matches!(op, IterOp::CountOnly | IterOp::BoolOnly) {
            let count = self
                .iterator_count_only_from_attrs(&attributes.to_map())?
                .unwrap_or_else(|| Value::int(0));
            return Ok(match op {
                IterOp::CountOnly => count,
                _ => Value::truth(count.to_f64() > 0.0),
            });
        }
        // The items array is read in place through a handle clone: copying it
        // per call made a `pull-one` loop over n elements O(n^2) (#16475).
        let (shared, index, lazy_source) = {
            let map = attributes.as_map();
            let shared = match map.get("items").map(Value::view) {
                Some(ValueView::Array(values, ..)) => Some(crate::gc::Gc::clone(&values)),
                _ => None,
            };
            let index = match map.get("index").map(Value::view) {
                Some(ValueView::Int(i)) if i >= 0 => i as usize,
                _ => 0,
            };
            (shared, index, map.get("lazy_source").cloned())
        };
        let shared_len = shared.as_ref().map_or(0, |a| a.as_slice().len());
        // A lazy source may not have produced the elements this call needs yet;
        // the grown prefix is committed along with the cursor.
        let topped_up = self.iterator_topup_from_lazy_source(
            lazy_source.as_ref(),
            op,
            index,
            args,
            shared_len,
        )?;
        let items: &[Value] = match (&topped_up, &shared) {
            (Some(more), _) => more.as_slice(),
            (None, Some(a)) => a.as_slice(),
            (None, None) => &[],
        };
        let Some(step) = step(op, items, index, args) else {
            return Ok(Value::iteration_end());
        };
        if let Some(range) = step.append.clone() {
            let vals = items[range].to_vec();
            self.iterator_append_to_array_arg(args, &vals);
        }
        let mut ops = vec![(
            Symbol::intern("index"),
            Some(Value::int(step.new_index as i64)),
        )];
        if let Some(more) = topped_up {
            ops.push((Symbol::intern("items"), Some(Value::array(more))));
        }
        attributes.write_keys(ops);
        Ok(step.ret)
    }

    /// The next element a `squish` scan keeps, or `IterationEnd`.
    // Cost: O(d), d = duplicates skipped, each one or two callback calls.
    fn squish_pull_one(&mut self, scan: &mut SquishScan) -> Result<Value, RuntimeError> {
        let key_of = |this: &mut Self, item: &Value| match scan.as_func.clone() {
            Some(func) => this.call_sub_value(func, vec![item.clone()]),
            None => Ok(item.clone()),
        };
        if !scan.initialized {
            let Some(first) = scan.source.first().cloned() else {
                return Ok(Value::iteration_end());
            };
            scan.prev_key = key_of(self, &first)?;
            scan.initialized = true;
            scan.scan_index = 1;
            return Ok(first);
        }
        while scan.scan_index < scan.source.len() {
            let item = scan.source[scan.scan_index].clone();
            let key = key_of(self, &item)?;
            let duplicate = match scan.with_func.clone() {
                Some(func) => self
                    .call_sub_value(func, vec![scan.prev_key.clone(), key.clone()])?
                    .truthy(),
                None => crate::runtime::values_identical(&scan.prev_key, &key),
            };
            scan.prev_key = key;
            scan.scan_index += 1;
            if !duplicate {
                return Ok(item);
            }
        }
        Ok(Value::iteration_end())
    }

    /// A protocol call on a `squish` scan, in terms of its `pull-one`. A scan
    /// is not a `PredictiveIterator` (Rakudo's `Any::Squish-With` is not), so
    /// `count-only` and `bool-only` never reach it.
    // Cost: O(k) pulls, k = elements the call consumes.
    fn squish_iterator_call(
        &mut self,
        scan: &mut SquishScan,
        op: IterOp,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let count_arg = |v: Option<&Value>, default: i64| {
            v.map(super::to_int).unwrap_or(default).max(0) as usize
        };
        Ok(match op {
            IterOp::PullOne => self.squish_pull_one(scan)?,
            IterOp::SkipOne => {
                Value::int(i64::from(!self.squish_pull_one(scan)?.is_iteration_end()))
            }
            IterOp::SkipAtLeast => {
                let mut ok = true;
                for _ in 0..count_arg(args.first(), 0) {
                    if self.squish_pull_one(scan)?.is_iteration_end() {
                        ok = false;
                        break;
                    }
                }
                Value::int(i64::from(ok))
            }
            IterOp::SkipAtLeastPullOne => {
                for _ in 0..count_arg(args.first(), 0) {
                    if self.squish_pull_one(scan)?.is_iteration_end() {
                        return Ok(Value::iteration_end());
                    }
                }
                self.squish_pull_one(scan)?
            }
            IterOp::PushExactly | IterOp::PushAtLeast => {
                let want = count_arg(args.get(1), 1);
                let mut collected = Vec::new();
                while collected.len() < want {
                    let next = self.squish_pull_one(scan)?;
                    if next.is_iteration_end() {
                        break;
                    }
                    collected.push(next);
                }
                self.iterator_append_to_array_arg(args, &collected);
                if collected.len() < want {
                    Value::iteration_end()
                } else {
                    Value::int(collected.len() as i64)
                }
            }
            IterOp::PushAll | IterOp::PushUntilLazy | IterOp::SinkAll => {
                let mut collected = Vec::new();
                loop {
                    let next = self.squish_pull_one(scan)?;
                    if next.is_iteration_end() {
                        break;
                    }
                    if op != IterOp::SinkAll {
                        collected.push(next);
                    }
                }
                self.iterator_append_to_array_arg(args, &collected);
                Value::iteration_end()
            }
            // A scan has no `PredictiveIterator` rows; reached only by a caller
            // that bypasses `owners_of`.
            IterOp::CountOnly | IterOp::BoolOnly => {
                return Err(crate::runtime::did_you_mean::method_not_found(
                    if op == IterOp::CountOnly {
                        "count-only"
                    } else {
                        "bool-only"
                    },
                    "Iterator",
                ));
            }
        })
    }
}
