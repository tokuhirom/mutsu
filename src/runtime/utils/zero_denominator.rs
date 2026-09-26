//! Find a zero-denominator Rational anywhere inside a value that is about to be
//! stringified (`.gist` / `.Str` / `say` / `put`).
//!
//! Rakudo defers `1/0`'s error until the Rational is coerced to a string, and a
//! collection's `.gist`/`.Str` coerces every rendered element, so
//! `say {a => 1/0}` and `say [1/0]` die with `Attempt to divide 1 by zero when
//! coercing Rational to Str` exactly like `say 1/0`. mutsu's pure renderers are
//! infallible (the element gists as `Inf`), so the callers ask this walk first.

use crate::runtime::RuntimeError;
use crate::value::{Value, ValueView};
use num_traits::Zero;
use std::collections::HashSet;

/// Nesting depth past which the walk gives up (the pure renderers bound their
/// own recursion; a structure this deep is not rendered element-by-element).
const MAX_DEPTH: usize = 256;

/// The `X::Numeric::DivideByZero` a zero-denominator Rational in `v` throws when
/// `v` is stringified, or `None` when `v` holds none. Descends through the
/// plain aggregates the native renderers expand (Array/List/Seq/Slip, Hash
/// values and object-hash keys, Pair key and value, Junction eigenstates,
/// item/cell containers); never into a lazy list (rendered as a placeholder)
/// or an object instance (rendered by its own `.gist`/`.Str`).
// Cost: O(t), t = elements reachable through plain aggregates; each Array/Hash
// is walked once (`seen`), so a cycle or a shared DAG node costs one visit.
pub(crate) fn zero_denominator_rational_error(v: &Value) -> Option<RuntimeError> {
    find(v, &mut HashSet::new(), 0)
}

fn find(v: &Value, seen: &mut HashSet<usize>, depth: usize) -> Option<RuntimeError> {
    if depth > MAX_DEPTH {
        return None;
    }
    let next = depth + 1;
    match v.view() {
        ValueView::Rat(n, 0) | ValueView::FatRat(n, 0) => {
            Some(RuntimeError::rational_to_str_divide_by_zero(Value::int(n)))
        }
        ValueView::BigRat(n, d) if d.is_zero() => Some(
            RuntimeError::rational_to_str_divide_by_zero(Value::from_bigint(n.clone())),
        ),
        ValueView::Scalar(inner) => find(inner, seen, next),
        ValueView::ContainerRef(cell) => {
            // Clone out and drop the guard before recursing: a cycle can
            // close through a cell (`my @e; @e.push(@e)`).
            let inner = cell.lock().unwrap().clone();
            find(&inner, seen, next)
        }
        ValueView::Array(data, kind) => {
            if kind.is_lazy() || !seen.insert(crate::gc::Gc::as_ptr(&data) as usize) {
                return None;
            }
            data.iter().find_map(|e| find(e, seen, next))
        }
        ValueView::Seq(items) | ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => {
            if items.is_lazy() {
                return None;
            }
            items.iter().find_map(|e| find(e, seen, next))
        }
        ValueView::Slip(items) => items.iter().find_map(|e| find(e, seen, next)),
        ValueView::Hash(map) => {
            if !seen.insert(crate::gc::Gc::as_ptr(&map) as usize) {
                return None;
            }
            if map.has_typed_keys()
                && let Some(err) = map.keys().find_map(|k| find(&map.typed_key(k), seen, next))
            {
                return Some(err);
            }
            map.values().find_map(|e| find(e, seen, next))
        }
        ValueView::Pair(_, val) => find(val, seen, next),
        ValueView::ValuePair(k, val) => find(k, seen, next).or_else(|| find(val, seen, next)),
        ValueView::Junction { values, .. } => values.iter().find_map(|e| find(e, seen, next)),
        _ => None,
    }
}

/// The single guard every string-context coercion that bypasses method
/// dispatch runs before rendering: prefix `~` (`StrCoerce`), infix `~` and the
/// string comparators (`coerce_stringy_operand`), interpolation
/// (`StringConcat`) and `join`. They render through the infallible
/// `to_str_context`, which prints a zero-denominator Rational as `Inf`; Rakudo
/// dies with `Attempt to divide 1 by zero when coercing Rational to Str`
/// (GH #9621). A plain Str and a lazy Match are tag-probed out first: they are
/// the hot operands here, and `view()` on a lazy Match would materialize it.
// Cost: O(1) for a scalar operand; O(t) for an aggregate (see
// `zero_denominator_rational_error`), already the cost of rendering it.
pub(crate) fn check_str_coercion_zero_denominator(v: &Value) -> Result<(), RuntimeError> {
    if v.is_str_value() || v.is_lazy_match_value() {
        return Ok(());
    }
    match zero_denominator_rational_error(v) {
        Some(err) => Err(err),
        None => Ok(()),
    }
}
