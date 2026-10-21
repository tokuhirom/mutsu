//! A list method called on a multi-dimensional shaped array iterates its
//! leaves (#14248).
//!
//! Rakudo's `Array.list` is the array itself, and a shaped array's iterator
//! walks its leaves in row-major order, so `@c.sum`, `@c.unique` or `@c.Bag`
//! over `my @c[2;2]` see four elements, not two rows. mutsu's list methods
//! read an Array's items directly, which for a shaped array are its rows. A
//! method of [`ITERATING_METHODS`] therefore receives the List of the leaves
//! instead, the way a lazy-tail Slip receives the list it reifies from
//! (`vm_lazy_slip.rs`).

use super::*;

/// The `Any`/`List` methods that read their receiver through its iterator
/// and that Rakudo answers over a shaped array's leaves. Methods about the
/// array itself (`elems`, `shape`, `keys`, the `*-POS` accessors, `raku`)
/// are absent, as are those Rakudo refuses on a shaped array (`reverse`,
/// `rotate`, `tail`).
const ITERATING_METHODS: &[&str] = &[
    "sum",
    "max",
    "min",
    "minmax",
    "unique",
    "repeated",
    "squish",
    "first",
    "head",
    "skip",
    "reduce",
    "produce",
    "Slip",
    "Bag",
    "BagHash",
    "Set",
    "SetHash",
    "Mix",
    "MixHash",
    "any",
    "all",
    "one",
    "none",
    "classify",
    "categorize",
    "pick",
    "roll",
    "combinations",
    "permutations",
];

impl Interpreter {
    /// The receiver a method call on `target` reads: the List of a
    /// multi-dimensional shaped array's leaves for a method of
    /// [`ITERATING_METHODS`], or `target` unchanged.
    // Cost: O(1) for any other receiver or method; O(E) to collect the
    // leaves, E = leaves.
    pub(crate) fn shaped_iteration_receiver(target: Value, method: &str) -> Value {
        if !ITERATING_METHODS.contains(&method) {
            return target;
        }
        match crate::value::shaped_array::shaped_array_iteration_leaves(&target) {
            Some(leaves) => Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(leaves)),
                crate::value::ArrayKind::List,
            ),
            None => target,
        }
    }
}
