//! The sorting algorithms behind `.sort`: the stable merge sort that calls a
//! user comparator in a fixed (left, right) order, and the adaptive sort used
//! when the comparison is a pure total order mutsu evaluates itself.

use crate::value::Value;
use std::cmp::Ordering;

/// Cost: O(e log e) comparator calls and O(e log e) `usize` moves, plus O(e)
/// `Value` clones to apply the permutation, e = elements. One scratch buffer of
/// e indices is allocated for the whole sort.
///
/// Stable merge sort that always calls the comparator with (left, right)
/// ordering, matching Raku's requirement that the comparator sees elements in
/// their relative position order. It sorts a permutation, so a level of the
/// merge moves indices, not `Value`s.
pub(crate) fn merge_sort_with_cmp<F>(items: &mut [Value], cmp: &mut F)
where
    F: FnMut(&Value, &Value) -> Ordering,
{
    if items.len() <= 1 {
        return;
    }
    let mut perm: Vec<usize> = (0..items.len()).collect();
    merge_sort_indices(&mut perm, items, cmp);
    apply_permutation(items, &perm);
}

/// Reorder `items` so that `items[k]` becomes the old `items[perm[k]]`.
fn apply_permutation(items: &mut [Value], perm: &[usize]) {
    if perm.iter().enumerate().all(|(k, &i)| k == i) {
        return;
    }
    let sorted: Vec<Value> = perm.iter().map(|&i| items[i].clone()).collect();
    for (slot, v) in items.iter_mut().zip(sorted) {
        *slot = v;
    }
}

/// Cost: O(e log e) comparator calls, e = `perm.len()`; one scratch buffer.
///
/// Stable merge sort over a permutation array, comparing the underlying
/// values. The sequence of comparator calls is that of a plain top-down merge
/// sort: each merge compares the head of the left run against the head of the
/// right run until one is exhausted.
pub(crate) fn merge_sort_indices<F>(perm: &mut [usize], items: &[Value], cmp: &mut F)
where
    F: FnMut(&Value, &Value) -> Ordering,
{
    let mut scratch = vec![0usize; perm.len()];
    merge_sort_indices_into(perm, &mut scratch, items, cmp);
}

fn merge_sort_indices_into<F>(perm: &mut [usize], scratch: &mut [usize], items: &[Value], cmp: &mut F)
where
    F: FnMut(&Value, &Value) -> Ordering,
{
    let len = perm.len();
    if len <= 1 {
        return;
    }
    let mid = len / 2;
    {
        let (pl, pr) = perm.split_at_mut(mid);
        let (sl, sr) = scratch.split_at_mut(mid);
        merge_sort_indices_into(pl, sl, items, cmp);
        merge_sort_indices_into(pr, sr, items, cmp);
    }
    scratch[..len].copy_from_slice(perm);
    let (left, right) = scratch[..len].split_at(mid);
    let (mut i, mut j, mut k) = (0, 0, 0);
    while i < left.len() && j < right.len() {
        if cmp(&items[left[i]], &items[right[j]]) != Ordering::Greater {
            perm[k] = left[i];
            i += 1;
        } else {
            perm[k] = right[j];
            j += 1;
        }
        k += 1;
    }
    if i < left.len() {
        perm[k..].copy_from_slice(&left[i..]);
    } else if j < right.len() {
        perm[k..].copy_from_slice(&right[j..]);
    }
}

/// The keys of a sort whose comparison is a total order on machine values:
/// every element is a plain `Int`, or every element is a plain `Str`.
pub(crate) enum PureSortKeys {
    Int(Vec<i64>),
    /// The decontainerized elements, each a `Str`.
    Str(Vec<Value>),
}

/// Cost: O(e), e = elements.
///
/// Classify `items` for [`pure_sort`]. `numeric` is a `<=>` comparison, else a
/// `cmp`. `None` when an element is anything else (a container, a `Num` that
/// may be NaN, a big `Int`, an `Instance`), and the caller keeps its general
/// comparator.
pub(crate) fn pure_sort_keys(items: &[Value], numeric: bool) -> Option<PureSortKeys> {
    // An array element sits in its Scalar container: compare the value in it.
    if numeric {
        let keys: Option<Vec<i64>> = items
            .iter()
            .map(|v| v.with_deref(|x| x.as_int()))
            .collect();
        keys.map(PureSortKeys::Int)
    } else {
        let keys: Option<Vec<Value>> = items
            .iter()
            .map(|v| {
                v.with_deref(|x| x.as_str().is_some().then(|| x.clone()))
            })
            .collect();
        keys.map(PureSortKeys::Str)
    }
}

/// Cost: O(e log e) comparisons in the worst case and O(e) on input made of a
/// few ascending or strictly descending runs, e = elements.
///
/// Sort `items` stably by a total order mutsu evaluates itself, so no user
/// code observes the order of comparisons and the standard library's adaptive
/// stable sort gives the same result as [`merge_sort_with_cmp`]. `reverse`
/// swaps the operands (`{ $^b <=> $^a }`).
pub(crate) fn pure_sort(items: &mut [Value], keys: PureSortKeys, reverse: bool) {
    let mut perm: Vec<usize> = (0..items.len()).collect();
    match keys {
        PureSortKeys::Int(keys) => {
            if reverse {
                perm.sort_by(|&i, &j| keys[j].cmp(&keys[i]));
            } else {
                perm.sort_by(|&i, &j| keys[i].cmp(&keys[j]));
            }
        }
        PureSortKeys::Str(keys) => {
            let str_cmp = |i: usize, j: usize| match (keys[i].as_str(), keys[j].as_str()) {
                (Some(x), Some(y)) => x.cmp(y),
                // `pure_sort_keys` checked that every key is a Str.
                _ => Ordering::Equal,
            };
            if reverse {
                perm.sort_by(|&i, &j| str_cmp(j, i));
            } else {
                perm.sort_by(|&i, &j| str_cmp(i, j));
            }
        }
    }
    apply_permutation(items, &perm);
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ints(v: &[i64]) -> Vec<Value> {
        v.iter().map(|&n| Value::int(n)).collect()
    }

    fn as_ints(v: &[Value]) -> Vec<i64> {
        v.iter().map(|x| x.as_int().unwrap()).collect()
    }

    #[test]
    fn merge_sort_matches_std_stable_sort() {
        let mut v = ints(&[5, 3, 9, 1, 3, 7, 0, 2, 8, 6, 4]);
        merge_sort_with_cmp(&mut v, &mut |a, b| {
            a.as_int().cmp(&b.as_int())
        });
        assert_eq!(as_ints(&v), vec![0, 1, 2, 3, 3, 4, 5, 6, 7, 8, 9]);
    }

    #[test]
    fn pure_int_sort_reverse() {
        let mut v = ints(&[1, 2, 3, 4, 5]);
        let keys = pure_sort_keys(&v, true).unwrap();
        pure_sort(&mut v, keys, true);
        assert_eq!(as_ints(&v), vec![5, 4, 3, 2, 1]);
    }
}
