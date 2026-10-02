//! Lazy iteration of an upward-unbounded `Range` (`1..*`, `^Inf`, `1.5..*`,
//! `1e0..Inf`, `"a"..*`), for every element type through one mechanism.
//!
//! A Range iterates by `.succ` from its (possibly excluded) start, so the
//! element type never needs its own lazy path:
//!
//! - [`lazy_list`] is the range as a reify-on-demand `LazyList` whose next
//!   element is the previous one's `.succ` (`SequenceSpec::Succ`). Contexts
//!   that keep their elements anyway (`my @a = 1..*`, `.List`, `.head`,
//!   `.first`, a subscript) use it instead of reifying a capped prefix.
//! - [`nth`] is the random-access form a lazy pipe (`.map`, `.grep`, `.pairs`,
//!   adaptors) pulls with, so a long-running pipe over a numeric range keeps no
//!   cache of its source. A non-numeric start (`"a"..*`) has no random access,
//!   so [`pipe_source`] hands such a pipe the `LazyList` instead.

use crate::value::{LazyList, SequenceSpec, Value, ValueView};

/// The first element of `range` when it is unbounded upward and iterates
/// (its start has a `.succ`); `None` for anything else, including a range
/// whose start is `+Inf` (`Inf..Inf` is empty).
///
/// Cost: O(1) for a numeric start; O(n) for a Str start, n = chars.
pub(crate) fn first(range: &Value) -> Option<Value> {
    if !crate::builtins::is_infinite_range(range) {
        return None;
    }
    let (start, excl_start) = match range.view() {
        ValueView::Range(a, _) | ValueView::RangeExcl(a, _) => (Value::int(a), false),
        ValueView::RangeExclStart(a, _) | ValueView::RangeExclBoth(a, _) => (Value::int(a), true),
        ValueView::GenericRange {
            start, excl_start, ..
        } => (start.as_ref().clone(), excl_start),
        _ => return None,
    };
    if matches!(start.view(), ValueView::Num(f) if f == f64::INFINITY) {
        return None;
    }
    // A start without a successor (an instance, a type object) cannot be
    // stepped; leave it to the eager path.
    let second = crate::builtins::value_succ(&start)?;
    Some(if excl_start { second } else { start })
}

/// Element `idx` of the range starting at `first` (from [`first`]), when the
/// start is numeric: `first + idx`, in the start's own type (Int, Rat, Num,
/// ...; `-Inf`/`NaN` stay put, as their `.succ` does). `None` for a
/// non-numeric start, which only steps by `.succ`.
///
/// Cost: O(1).
pub(crate) fn nth(first: &Value, idx: usize) -> Option<Value> {
    if !first.is_numeric() {
        return None;
    }
    if idx == 0 {
        return Some(first.clone());
    }
    let offset = i64::try_from(idx).map_or_else(
        |_| Value::from_bigint(num_bigint::BigInt::from(idx)),
        Value::int,
    );
    crate::builtins::arith_add(first.clone(), offset).ok()
}

/// `range` as a reify-on-demand `LazyList` stepping by `.succ`; `None` when
/// [`first`] is.
///
/// Cost: O(1) to build; each element is produced once, on demand.
pub(crate) fn lazy_list(range: &Value) -> Option<LazyList> {
    Some(LazyList::new_sequence(
        vec![first(range)?],
        SequenceSpec::Succ,
    ))
}

/// The source a lazy pipe stage should pull `source` through: an unbounded
/// range with a non-numeric start becomes its [`lazy_list`] (it has no
/// [`nth`]); anything else is returned unchanged.
///
/// Cost: O(1).
pub(crate) fn pipe_source(source: Value) -> Value {
    match first(&source) {
        Some(start) if !start.is_numeric() => match lazy_list(&source) {
            Some(ll) => Value::lazy_list(crate::gc::Gc::new(ll)),
            None => source,
        },
        _ => source,
    }
}

/// Walks an unbounded range from its [`first`] element by `.succ`, holding only
/// the next element: what a consumer that stops on its own (`.head(n)`,
/// `.first`, a `for` loop with `last`) iterates with, in O(1) memory.
pub(crate) struct Steps {
    next: Option<Value>,
}

impl Steps {
    /// `None` when `range` is not an unbounded range ([`first`]).
    pub(crate) fn new(range: &Value) -> Option<Self> {
        Some(Self {
            next: Some(first(range)?),
        })
    }

    /// The next `n` elements.
    ///
    /// Cost: O(n) for a numeric range; O(n * c) for a Str one, c = chars.
    pub(crate) fn take(&mut self, n: usize) -> Vec<Value> {
        let mut out = Vec::with_capacity(n.min(4096));
        while out.len() < n {
            let Some(cur) = self.next.take() else { break };
            self.next = crate::builtins::value_succ(&cur);
            out.push(cur);
        }
        out
    }
}
