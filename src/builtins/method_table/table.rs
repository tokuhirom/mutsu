//! The lookup structure behind the built-in method table (ADR-11276 §2.6):
//! every group's rows, resolved once per process along each shape's MRO.

use super::table_const::{
    ALL, ARITIES, ENTRY_IDS, ENTRY_KEYS, ENTRY_START, MUT_ARITIES, NAME_ROWS, NAME_START, NAMES,
    SHAPES, TYPE_SHAPES,
};
use super::{MethodRow, RowFlags};
use crate::symbol::Symbol;
use crate::value::{DispatchShape, Value};
use std::cell::RefCell;

use super::{collections, ctors_mop, instances, io_concurrency, mutating, scalars};

/// Every group's families. A group is one slice of ADR-11276 §10 and owns a
/// directory of its own, so a slice adds its rows to its group's
/// `FAMILIES` and never edits this list: the groups are listed here once, in
/// slice 3A.
pub(super) static GROUPS: &[&[&[MethodRow]]] = &[
    scalars::FAMILIES,
    collections::FAMILIES,
    instances::FAMILIES,
    io_concurrency::FAMILIES,
    mutating::FAMILIES,
    ctors_mop::FAMILIES,
];

/// Every row of every group, in registration order.
#[cfg(test)]
pub(super) fn all_rows() -> impl Iterator<Item = &'static MethodRow> {
    ALL.iter().copied()
}

/// What a call's receiver is to the table: an instance of a shape, or the type
/// object of a built-in type (`Int` in `Int.Bool`). A type object answers only
/// the rows flagged [`RowFlags::TYPE_OBJECT_OK`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct Receiver {
    pub(crate) shape: DispatchShape,
    pub(crate) type_object: bool,
}

impl Receiver {
    /// The receiver of an instance of `shape`.
    // Cost: O(1).
    pub(crate) const fn instance(shape: DispatchShape) -> Receiver {
        Receiver {
            shape,
            type_object: false,
        }
    }

    /// The receiver of the type object of `shape`'s type.
    // Cost: O(1).
    pub(crate) const fn type_object(shape: DispatchShape) -> Receiver {
        Receiver {
            shape,
            type_object: true,
        }
    }

    /// What `target` is to the table, or `None` when the table does not
    /// answer calls on it: it is neither a plain instance of a shape nor the
    /// type object of one.
    // Cost: O(1), a tag probe; a `Package` also looks its name up.
    #[inline]
    pub(crate) fn of(target: &Value) -> Option<Receiver> {
        match target.dispatch_shape() {
            Some(shape) => Some(Receiver::instance(shape)),
            None => target.type_object_shape().map(Receiver::type_object),
        }
    }

    /// The receiver packed into a byte: the shape and a type-object bit.
    // Cost: O(1).
    pub(crate) const fn to_bits(self) -> u8 {
        self.shape as u8 | ((self.type_object as u8) << 7)
    }
}

/// A row's index in the table: what a call site's inline cache remembers
/// (`vm_method_site_lane`). Stable for the life of the process.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct RowId(u16);

impl RowId {
    /// The id as the `u16` a site cache word packs.
    pub(crate) fn to_bits(self) -> u16 {
        self.0
    }

    /// The id a site cache word packed with [`Self::to_bits`]. Only ever
    /// given bits that came from `to_bits` in this process.
    pub(crate) fn from_bits(bits: u16) -> Self {
        Self(bits)
    }
}

/// The memo's marker for a symbol not yet looked up.
const UNKNOWN: u16 = u16::MAX;
/// The memo's marker for a symbol that names no row.
const NO_NAME: u16 = u16::MAX - 1;

thread_local! {
    /// Per symbol id, its index in [`NAMES`] (or a marker). A symbol is
    /// interned at run time, so the compile-time index cannot be keyed by it;
    /// each distinct name pays one binary search per thread, the first time it
    /// is asked about.
    static NAME_MEMO: RefCell<Vec<u16>> = const { RefCell::new(Vec::new()) };
}

/// `method`'s index in [`NAMES`], if some row is named so.
// Cost: O(1) after the first ask per thread; O(log u * len), u = distinct row
// names, len = name length, on the first.
#[inline]
fn name_index(method: Symbol) -> Option<usize> {
    let id = method.id() as usize;
    let cached = NAME_MEMO.try_with(|memo| memo.borrow().get(id).copied());
    match cached {
        Ok(Some(idx)) if idx != UNKNOWN => return (idx != NO_NAME).then_some(usize::from(idx)),
        _ => {}
    }
    let found = NAMES.binary_search(&method.as_str()).ok();
    let marker = found.map_or(NO_NAME, |idx| idx as u16);
    // A thread that is being torn down has no memo; the answer stands without it.
    let _ = NAME_MEMO.try_with(|memo| {
        let mut memo = memo.borrow_mut();
        if memo.len() <= id {
            memo.resize(id + 1, UNKNOWN);
        }
        memo[id] = marker;
    });
    found
}

/// Whether some row is named `method` and takes `arity` arguments: the test
/// every lookup makes first.
// Cost: O(1), a memo read and a bit test.
#[inline]
pub(crate) fn names_a_row(method: Symbol, arity: usize) -> bool {
    arity < 8 && name_index(method).is_some_and(|name| ARITIES[name] & (1 << arity) != 0)
}

/// Whether any mutating row is named `method` and takes `arity` arguments:
/// the test every [`invoke_mut`](super::invoke_mut) call makes first. A call
/// longer than the masks go answers by bit 7, which only a slurpy row sets.
// Cost: O(1), a memo read and a bit test.
#[inline]
pub(crate) fn names_a_mut_row(method: Symbol, arity: usize) -> bool {
    name_index(method).is_some_and(|name| MUT_ARITIES[name] & (1 << arity.min(7)) != 0)
}

/// Whether a receiver of `receiver` may have a row for `method`: the second
/// bit test, once the receiver is known. `false` is definite.
// Cost: O(1), a memo read and a bit test.
#[inline]
pub(crate) fn shape_has_row(receiver: Receiver, method: Symbol) -> bool {
    let Some(name) = name_index(method) else {
        return false;
    };
    let bits = if receiver.type_object {
        TYPE_SHAPES[name]
    } else {
        SHAPES[name]
    };
    bits & (1 << (receiver.shape as u64)) != 0
}

/// The row a receiver dispatches `method` to when called with `arity`
/// positional arguments, if the table has one.
// Cost: O(log e), e = entries of the method's name (a memo read, two bit
// tests and a binary search).
#[inline]
pub(crate) fn resolve(receiver: Receiver, method: Symbol, arity: usize) -> Option<RowId> {
    let name = name_index(method)?;
    if arity >= 8 || ARITIES[name] & (1 << arity) == 0 || !shape_has_row(receiver, method) {
        return None;
    }
    let key = u16::from(receiver.to_bits()) << 8 | arity as u16;
    let (lo, hi) = (
        usize::from(ENTRY_START[name]),
        usize::from(ENTRY_START[name + 1]),
    );
    let at = ENTRY_KEYS[lo..hi].binary_search(&key).ok()?;
    Some(RowId(ENTRY_IDS[lo + at]))
}

/// The ids of the rows named `NAMES[name]`, in registration order.
// Cost: O(1).
fn rows_named(name: usize) -> &'static [u16] {
    &NAME_ROWS[usize::from(NAME_START[name])..usize::from(NAME_START[name + 1])]
}

/// The row `owner` declares for `method` taking `arity` positional arguments,
/// whatever the receiver is: the lookup of a receiver that has no shape. A
/// slurpy row answers every arity from its own up, however long.
// Cost: O(k), k = rows named `method` (a string compare of the owner each).
pub(crate) fn owner_row(owner: Symbol, method: Symbol, arity: usize) -> Option<RowId> {
    let rows = rows_named(name_index(method)?);
    let owner = owner.as_str();
    let declared = || {
        rows.iter()
            .copied()
            .filter(|&id| ALL[usize::from(id)].owner == owner)
    };
    if let Some(arity) = u8::try_from(arity).ok()
        && let Some(id) = declared().find(|&id| ALL[usize::from(id)].arities().contains(&arity))
    {
        return Some(RowId(id));
    }
    let id = declared().find(|&id| ALL[usize::from(id)].flags.contains(RowFlags::SLURPY))?;
    (arity >= usize::from(ALL[usize::from(id)].arity)).then_some(RowId(id))
}

/// The row `id` names.
// Cost: O(1).
#[inline]
pub(crate) fn row(id: RowId) -> &'static MethodRow {
    ALL[usize::from(id.0)]
}

/// The row a receiver dispatches `method` to, if any (the lookup
/// [`super::try_dispatch`] makes, for tests).
#[cfg(test)]
pub(super) fn lookup(receiver: Receiver, method: Symbol, arity: u8) -> Option<&'static MethodRow> {
    resolve(receiver, method, usize::from(arity)).map(row)
}
