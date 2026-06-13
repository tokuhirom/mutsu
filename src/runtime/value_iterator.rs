//! Unified pull-model iterator over `Value` sources.
//!
//! This is the skeleton of the lazy-list / Iterator unification (PLAN.md §8.1).
//! Raku's iteration is pull-based (`Iterator.pull-one` returns the next value or
//! the `IterationEnd` sentinel). mutsu historically grew ~7 ad-hoc iteration
//! mechanisms with inconsistent guards, so an infinite `Range` (`1..Inf`, stored
//! as `Value::Range(1, i64::MAX)`) reaching an unguarded `(a..=b).collect()`
//! panics the whole process with `capacity overflow` (ANALYSIS §8.2).
//!
//! `ValueIterator` is the single place where a `Value` is turned into a stream of
//! elements pulled one at a time. PR-1 implements the *pure* sources (those that
//! need no VM/Interpreter callback): integer ranges and already-materialized
//! slices. Sources that require running bytecode to produce the next element
//! (gather coroutines, lazy map, sequence specs, string succession, hashes) are
//! reported by [`ValueIterator::from_value`] returning `None`; the caller then
//! falls back to the existing `value_to_list`. Later PRs add VM-driven variants
//! that pull those lazily via the VM's `force_lazy_list_vm_n`.

use crate::value::Value;
use std::sync::Arc;

/// A pull source over `Value` elements.
pub(crate) enum ValueIterator {
    /// Already-materialized elements (Seq / Slip). Shares the source `Arc`.
    Slice { items: Arc<Vec<Value>>, idx: usize },
    /// Already-materialized Array elements. Shares the source `Arc` (the
    /// `ArrayData` wrapper derefs to the element vector — no copy).
    ArraySlice {
        items: Arc<crate::value::ArrayData>,
        idx: usize,
    },
    /// Lazy integer counter. `end == i64::MAX` with `inclusive` represents an
    /// open-ended (infinite) range and must never be eagerly collected.
    IntRange { cur: i64, end: i64, inclusive: bool },
}

impl ValueIterator {
    /// Build a pull iterator for `val`, or `None` if `val` needs VM/Interpreter
    /// context (or per-type logic) to enumerate — in which case the caller falls
    /// back to [`crate::runtime::utils::value_to_list`].
    ///
    /// Range-family and already-materialized sequences become pure iterators;
    /// `LazyList`, `GenericRange`, `Hash` and scalars return `None`.
    pub(crate) fn from_value(val: &Value) -> Option<ValueIterator> {
        match val {
            // Already-materialized sequences: share the backing Arc, no copy.
            Value::Array(items, kind) if !kind.is_itemized() => Some(ValueIterator::ArraySlice {
                items: items.clone(),
                idx: 0,
            }),
            Value::Seq(items) | Value::Slip(items) => Some(ValueIterator::Slice {
                items: items.clone(),
                idx: 0,
            }),
            // Integer ranges: lazy counters. The only sources that can be
            // infinite via `i64::MAX` and thus the crash vector we must guard.
            Value::Range(a, b) => Some(ValueIterator::IntRange {
                cur: *a,
                end: *b,
                inclusive: true,
            }),
            Value::RangeExcl(a, b) => Some(ValueIterator::IntRange {
                cur: *a,
                end: *b,
                inclusive: false,
            }),
            Value::RangeExclStart(a, b) => Some(ValueIterator::IntRange {
                cur: *a + 1,
                end: *b,
                inclusive: true,
            }),
            Value::RangeExclBoth(a, b) => Some(ValueIterator::IntRange {
                cur: *a + 1,
                end: *b,
                inclusive: false,
            }),
            // LazyList, GenericRange, Hash, scalars, ...: need the interpreter/VM
            // or per-type logic to enumerate. Defer to value_to_list.
            _ => None,
        }
    }

    /// Pull the next value, or `None` when the source is exhausted.
    pub(crate) fn pull_one(&mut self) -> Option<Value> {
        match self {
            ValueIterator::Slice { items, idx } => {
                let v = items.get(*idx).cloned()?;
                *idx += 1;
                Some(v)
            }
            ValueIterator::ArraySlice { items, idx } => {
                let v = items.get(*idx).cloned()?;
                *idx += 1;
                Some(v)
            }
            ValueIterator::IntRange {
                cur,
                end,
                inclusive,
            } => {
                let in_bounds = if *inclusive {
                    *cur <= *end
                } else {
                    *cur < *end
                };
                if !in_bounds {
                    return None;
                }
                let v = Value::Int(*cur);
                match cur.checked_add(1) {
                    Some(next) => *cur = next,
                    // Reached i64::MAX: make the next pull terminate.
                    None => *inclusive = false,
                }
                Some(v)
            }
        }
    }
}

/// Materialize `val` to a `Vec`, truncating at `cap` elements.
///
/// This is the single guarded expansion point for the pure pull sources: an
/// infinite integer `Range` (`b == i64::MAX`) is truncated at `cap` instead of
/// panicking with `capacity overflow`. Sources that need VM/Interpreter context
/// (where [`ValueIterator::from_value`] returns `None`) fall back to the existing
/// [`crate::runtime::utils::value_to_list`], which has its own per-type guards.
///
/// Later PRs will replace the fallback with real VM-driven pulls and route the
/// remaining unguarded `(a..=b).collect()` sites here (PLAN.md §8.2).
pub(crate) fn materialize_capped(val: &Value, cap: usize) -> Vec<Value> {
    let Some(mut it) = ValueIterator::from_value(val) else {
        return crate::runtime::utils::value_to_list(val);
    };
    let mut out = Vec::new();
    while let Some(v) = it.pull_one() {
        if out.len() >= cap {
            break;
        }
        out.push(v);
    }
    out
}
