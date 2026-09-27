//! Native list iterators behind the lazy `Seq`s that an Array's
//! `.keys` / `.values` / `.kv` / `.pairs` / `.antipairs` / `.batch` and every list's
//! `.combinations` / `.permutations` return (#9158).
//!
//! This follows Rakudo, where each of these methods is
//! `Seq.new(<iterator class>.new(...))`: the Array views read the array
//! through a live index cursor (`my $v = @a.values; @a.push(4)` shows the
//! pushed element; only `.keys` fixes its count at the call, as Rakudo's
//! count-only iterator over `@a.elems` does), and the combinatorics iterators
//! step a lexicographic index vector over a snapshot of the invocant. So `.head(3)` pulls three
//! elements, and `(^10).permutations.head` builds one permutation, not 10!.
//!
//! Like [`crate::value::StrIterSpec`], a [`ListGen`] needs no interpreter:
//! the `Seq` holding it ([`crate::value::SeqSource::Pure`]) is cut on its
//! first read by whoever reads it, and only a consuming `.head(n)` / `.first`
//! or a subscript on an unread one stops after the prefix.

use crate::value::{Value, ValueView};
use std::sync::Arc;

/// Which positional view of an Array a [`ListGen::Positional`] produces.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PositionalMode {
    /// `.keys`: the index.
    Keys,
    /// `.values`: the element.
    Values,
    /// `.kv`: the index, then the element.
    Kv,
    /// `.pairs`: `index => element`.
    Pairs,
    /// `.antipairs`: `element => index`, with the element decontainerized and
    /// de-itemized (it becomes a key; see `positional_antipairs`).
    Antipairs,
}

/// Cursor state of one lazy list iterator (see the module docs).
#[derive(Debug, Clone)]
pub(crate) enum ListGen {
    /// A positional view of a live Array, read one index at a time.
    Positional {
        array: Value,
        mode: PositionalMode,
        /// Hand out each element's own container (promoting the slot in
        /// place) rather than its value — the element-producer contract of a
        /// mutable Array's `.values` / `.pairs` / `.kv`
        /// (`vm_element_producers.rs`, ADR-0036).
        cells: bool,
        /// The next index to read.
        pos: usize,
        /// `.keys` counts the elements there were at the call (Rakudo's
        /// `Array.keys` is a count-only iterator over `self.elems`); the other
        /// views re-read the live length on every pull.
        end: Option<usize>,
        /// `.kv`'s element half, when a pull stopped between an index and
        /// its element.
        pending: Option<Value>,
    },
    /// `.batch(size)` of a live Array: consecutive sub-lists of `size`
    /// elements, the last one possibly shorter.
    Batch {
        array: Value,
        size: usize,
        /// The index of the next batch's first element.
        pos: usize,
    },
    /// `.combinations(k)` for every `k` in `k..=k_max`, in Rakudo's order:
    /// by size, then lexicographically by index.
    Combinations {
        items: Arc<Vec<Value>>,
        /// The size of the combinations currently produced.
        k: usize,
        k_max: usize,
        /// The index vector of the next combination of size `k`; `None` once
        /// size `k` is exhausted (the next pull moves to `k + 1`).
        next: Option<Vec<usize>>,
    },
    /// `.permutations`, lexicographically by index (Rakudo's order).
    Permutations {
        items: Arc<Vec<Value>>,
        /// The index vector of the next permutation; `None` once exhausted.
        next: Option<Vec<usize>>,
    },
}

impl ListGen {
    /// A live positional view of `array` (which must be an Array value).
    // Cost: O(1).
    pub(crate) fn positional(array: Value, mode: PositionalMode, cells: bool) -> Self {
        let end = match (mode, array.view()) {
            (PositionalMode::Keys, ValueView::Array(items, _)) => Some(items.len()),
            _ => None,
        };
        ListGen::Positional {
            array,
            mode,
            cells,
            pos: 0,
            end,
            pending: None,
        }
    }

    /// `.batch(size)` of the live Array `array` (`size >= 1`).
    // Cost: O(1).
    pub(crate) fn batch(array: Value, size: usize) -> Self {
        ListGen::Batch {
            array,
            size: size.max(1),
            pos: 0,
        }
    }

    /// Every combination of `items` whose size lies in `k_min..=k_max`
    /// (clamped to `0..=items.len()`).
    // Cost: O(1) beyond the `items` snapshot the caller built.
    pub(crate) fn combinations(items: Vec<Value>, k_min: i64, k_max: i64) -> Self {
        let n = items.len() as i64;
        let lo = k_min.max(0);
        let hi = k_max.min(n);
        let (k, k_max, next) = if lo > hi {
            (1, 0, None)
        } else {
            let k = lo as usize;
            (k, hi as usize, Some((0..k).collect()))
        };
        ListGen::Combinations {
            items: Arc::new(items),
            k,
            k_max,
            next,
        }
    }

    /// Every permutation of `items`.
    // Cost: O(e), e = elements of `items` (the first index vector).
    pub(crate) fn permutations(items: Vec<Value>) -> Self {
        let next = Some((0..items.len()).collect());
        ListGen::Permutations {
            items: Arc::new(items),
            next,
        }
    }

    /// Whether every element this iterator yields is defined (an `Int`, a
    /// `Pair` or an `Array`) — false for the views that hand out elements.
    // Cost: O(1).
    pub(crate) fn never_nilish(&self) -> bool {
        !matches!(
            self,
            ListGen::Positional {
                mode: PositionalMode::Values | PositionalMode::Kv,
                ..
            }
        )
    }

    /// Rakudo's `pull-one`: the next element, or `None` at the end.
    // Cost: O(1) for a positional view; O(n) for a batch of n elements; O(k)
    // for a combination of size k;
    // O(e) for a permutation, e = elements (amortized O(1) index steps, plus
    // building the e-element result).
    pub(crate) fn pull_one(&mut self) -> Option<Value> {
        match self {
            ListGen::Positional {
                array,
                mode,
                cells,
                pos,
                end,
                pending,
            } => {
                if let Some(v) = pending.take() {
                    return Some(v);
                }
                let len = match (*end, array.view()) {
                    (Some(end), _) => end,
                    (None, ValueView::Array(items, _)) => items.len(),
                    (None, _) => 0,
                };
                if *pos >= len {
                    return None;
                }
                let i = *pos;
                *pos += 1;
                let key = Value::int(i as i64);
                if *mode == PositionalMode::Keys {
                    return Some(key);
                }
                let element = if *cells {
                    array
                        .array_slot_ref(i, true)
                        .unwrap_or_else(|| positional_item(array, i))
                } else {
                    positional_item(array, i)
                };
                Some(match mode {
                    PositionalMode::Keys => unreachable!("handled above"),
                    PositionalMode::Values if *cells => element,
                    PositionalMode::Values => element.deref_container(),
                    PositionalMode::Kv => {
                        *pending = Some(element);
                        key
                    }
                    PositionalMode::Pairs => Value::value_pair(key, element),
                    PositionalMode::Antipairs => positional_antipair(&element, i),
                })
            }
            ListGen::Batch { array, size, pos } => {
                let ValueView::Array(items, _) = array.view() else {
                    return None;
                };
                if *pos >= items.len() {
                    return None;
                }
                let end = (*pos + *size).min(items.len());
                let chunk = Value::array(items[*pos..end].to_vec());
                *pos = end;
                Some(chunk)
            }
            ListGen::Combinations {
                items,
                k,
                k_max,
                next,
            } => loop {
                if *k > *k_max {
                    return None;
                }
                let Some(idx) = next.as_mut() else {
                    *k += 1;
                    if *k <= *k_max {
                        *next = Some((0..*k).collect());
                    }
                    continue;
                };
                let n = items.len();
                let combo = Value::array(idx.iter().map(|&i| items[i].clone()).collect());
                if !advance_combination(idx, n) {
                    *next = None;
                }
                return Some(combo);
            },
            ListGen::Permutations { items, next } => {
                let idx = next.as_mut()?;
                let perm = Value::array(idx.iter().map(|&i| items[i].clone()).collect());
                if !advance_permutation(idx) {
                    *next = None;
                }
                Some(perm)
            }
        }
    }

    /// Append up to `n` more elements to `out` (Rakudo's `push-exactly`;
    /// `usize::MAX` is `push-all`).
    // Cost: `n` times `pull_one`'s cost.
    pub(crate) fn push_up_to(&mut self, out: &mut Vec<Value>, n: usize) {
        for _ in 0..n {
            match self.pull_one() {
                Some(v) => out.push(v),
                None => return,
            }
        }
    }

    /// Every `Value` edge this cursor retains, for the collector.
    // Cost: O(e), e = elements of a combinatorics snapshot; O(1) otherwise.
    pub(crate) fn trace_edges(&self, visit: &mut dyn FnMut(&crate::gc::ErasedGc)) {
        match self {
            ListGen::Batch { array, .. } => array.gc_trace(visit),
            ListGen::Positional { array, pending, .. } => {
                array.gc_trace(visit);
                if let Some(v) = pending {
                    v.gc_trace(visit);
                }
            }
            ListGen::Combinations { items, .. } | ListGen::Permutations { items, .. } => {
                for v in items.iter() {
                    v.gc_trace(visit);
                }
            }
        }
    }
}

/// `.antipairs`' pair for element `value` at index `idx`.
///
/// ADR-0040: `.antipairs` is `self.pairs.map: *.antipair`, and
/// `Pair.antipair` READS `$!value` to build the new key -- an attribute read
/// decontainerizes. So an element that is itemized because it is a real
/// `Array`/`Hash` element (slices 1-2) becomes a BARE key here, while
/// `.pairs` keeps it itemized as the pair's value. Measured:
/// `my @c; @c[0]=[1,2]; @c.antipairs.raku` is `([1, 2] => 0,).Seq` but
/// `@c.pairs.raku` is `(0 => $[1, 2],).Seq`. `deref_container` first: an
/// element promoted to its own `Scalar` container (ADR-0036 slice 3 hands
/// these out from `.pairs`) must be seen through before the de-itemization
/// can apply.
// Cost: O(1).
pub(crate) fn positional_antipair(value: &Value, idx: usize) -> Value {
    Value::value_pair(
        value.deref_container().deitemize_element(),
        Value::int(idx as i64),
    )
}

/// Element `i` of the Array `array`, as the eager `.values` / `.pairs` read it.
// Cost: O(1).
fn positional_item(array: &Value, i: usize) -> Value {
    match array.view() {
        ValueView::Array(items, _) => items.get(i).cloned().unwrap_or(Value::NIL),
        _ => Value::NIL,
    }
}

/// Step `idx` (a strictly increasing size-k index vector over `n` elements) to
/// the next combination in lexicographic order. `false` when it was the last.
// Cost: O(k), k = `idx.len()`.
fn advance_combination(idx: &mut [usize], n: usize) -> bool {
    let k = idx.len();
    let mut i = k;
    while i > 0 {
        i -= 1;
        if idx[i] != i + n - k {
            idx[i] += 1;
            for j in (i + 1)..k {
                idx[j] = idx[j - 1] + 1;
            }
            return true;
        }
    }
    false
}

/// Step `idx` to the next permutation in lexicographic order (Narayana
/// Pandita's algorithm). `false` when it was the last.
// Cost: O(e), e = `idx.len()`; amortized O(1) over a full enumeration.
fn advance_permutation(idx: &mut [usize]) -> bool {
    let n = idx.len();
    if n < 2 {
        return false;
    }
    let mut i = n - 1;
    while i > 0 && idx[i - 1] >= idx[i] {
        i -= 1;
    }
    if i == 0 {
        return false;
    }
    let mut j = n - 1;
    while idx[j] <= idx[i - 1] {
        j -= 1;
    }
    idx.swap(i - 1, j);
    idx[i..].reverse();
    true
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ints(it: &mut ListGen) -> Vec<Vec<i64>> {
        let mut out = Vec::new();
        it.push_up_to(&mut out, usize::MAX);
        out.iter()
            .map(|v| match v.view() {
                ValueView::Array(items, _) => items
                    .iter()
                    .map(|x| match x.view() {
                        ValueView::Int(i) => i,
                        _ => panic!("not an Int"),
                    })
                    .collect(),
                _ => panic!("not an Array"),
            })
            .collect()
    }

    fn items(n: i64) -> Vec<Value> {
        (0..n).map(Value::int).collect()
    }

    #[test]
    fn combinations_in_rakudo_order() {
        let mut it = ListGen::combinations(items(4), 2, 2);
        assert_eq!(
            ints(&mut it),
            vec![
                vec![0, 1],
                vec![0, 2],
                vec![0, 3],
                vec![1, 2],
                vec![1, 3],
                vec![2, 3]
            ]
        );
    }

    #[test]
    fn combinations_range_and_edges() {
        let mut it = ListGen::combinations(items(2), 0, 5);
        assert_eq!(
            ints(&mut it),
            vec![Vec::<i64>::new(), vec![0], vec![1], vec![0, 1]]
        );
        assert!(ints(&mut ListGen::combinations(items(2), 3, 3)).is_empty());
        assert_eq!(
            ints(&mut ListGen::combinations(items(0), 0, 0)),
            vec![Vec::<i64>::new()]
        );
    }

    #[test]
    fn permutations_in_rakudo_order() {
        let mut it = ListGen::permutations(items(3));
        assert_eq!(
            ints(&mut it),
            vec![
                vec![0, 1, 2],
                vec![0, 2, 1],
                vec![1, 0, 2],
                vec![1, 2, 0],
                vec![2, 0, 1],
                vec![2, 1, 0]
            ]
        );
        assert_eq!(
            ints(&mut ListGen::permutations(items(0))),
            vec![Vec::<i64>::new()]
        );
    }
}
