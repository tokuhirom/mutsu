//! The built-in method table (ADR-11276): one registered row per built-in
//! method, and invocation through that row.
//!
//! # What a row is
//!
//! A [`MethodRow`] is `(owner, name, arity, handler)`. `owner` is the type
//! Rakudo declares the method on (`elems` is `List`'s, so an `Array` finds it
//! one MRO level up), and `handler` is the method's implementation. The row is
//! the definition: a method in this table has no second copy in a dispatch
//! cascade. A cascade arm that still has to answer the same method for
//! receivers the table does not cover (a `Seq`, a `Buf`, an instance) calls
//! the same handler, so there is one implementation however the call arrives.
//!
//! # How a call reaches a row
//!
//! [`try_dispatch`] decodes the receiver's [`DispatchShape`] — a tag probe that
//! refuses everything the receiver-state checks in
//! `vm_native_dispatch::try_native_method_raw` exist for (instances, type
//! objects, mixins, containers, lazy and shaped values) — and looks
//! `(shape, method symbol, arity)` up in a map built once per process by
//! walking each shape's MRO (`builtin_types::catalog`) most-derived first. A
//! row is handed only plain scalar arguments ([`plain_args`]): named arguments
//! (which arrive as `Pair`s), Junctions, Failures, lazy Seqs and the like need
//! probes the table skips. A miss, a call with any other argument, or a
//! [`Handler::Narrow`] row that does not bind the arguments returns `None`, and
//! the call takes the cascades exactly as before; that fallback is what lets
//! the families migrate one at a time (ADR-11276 §6).
//!
//! # Adding a row
//!
//! Put the handler and its row in the family module of the type Rakudo
//! declares the method on, and make any cascade arm that answers the same
//! method call the handler. A pair may be added only when no probe the table
//! skips can claim it for a plain receiver of that shape — the probes are
//! listed in `try_native_method_raw`, `native_method_0arg`'s prologue and
//! `dispatch_core`'s prologue. In debug builds every table hit is re-answered
//! through the full pure path and the two must agree
//! ([`debug_assert_matches_full_path`]); CI's `debug-tap` job runs that over
//! the whole TAP suite. `rows_are_declared_by_rakudo` checks each row's owner.

mod collections;
mod ctors_mop;
mod dispatch;
mod instances;
mod io_concurrency;
mod mutating;
mod row;
mod scalars;

// The family modules keep their historical paths
// (`method_table::str::tclc`), whichever group directory holds them.
pub(crate) use collections::{
    any_collection, list, list_aggregate, list_transform, map, positional,
};
pub(crate) use scalars::{coerce, complex, real, str, str_iter, str_search, succ_pred};

#[cfg(test)]
pub(crate) use dispatch::try_dispatch;
pub(crate) use dispatch::{admits, answer, invoke, invoke_in, try_dispatch_in};
pub(crate) use row::{Handler, MethodRow, Named, RowFlags};

use crate::symbol::Symbol;
use crate::value::DispatchShape;
use rustc_hash::FxHashMap;
use std::sync::OnceLock;

/// Every group's families. A group is one slice of ADR-11276 §10 and owns a
/// directory of its own, so a slice adds its rows to its group's
/// `FAMILIES` and never edits this list: the groups are listed here once, in
/// slice 3A.
static GROUPS: &[&[&[MethodRow]]] = &[
    scalars::FAMILIES,
    collections::FAMILIES,
    instances::FAMILIES,
    io_concurrency::FAMILIES,
    mutating::FAMILIES,
    ctors_mop::FAMILIES,
];

/// Every row of every group, in registration order.
fn all_rows() -> impl Iterator<Item = &'static MethodRow> {
    GROUPS
        .iter()
        .flat_map(|families| families.iter())
        .flat_map(|rows| rows.iter())
}

/// The built-in type whose MRO a receiver of `shape` is dispatched along.
fn shape_type(shape: DispatchShape) -> &'static str {
    match shape {
        DispatchShape::List => "List",
        DispatchShape::Array => "Array",
        DispatchShape::Hash => "Hash",
        DispatchShape::Str => "Str",
        DispatchShape::Int => "Int",
        DispatchShape::Num => "Num",
        DispatchShape::Rat => "Rat",
        DispatchShape::FatRat => "FatRat",
        DispatchShape::Complex => "Complex",
    }
}

const SHAPES: [DispatchShape; 9] = [
    DispatchShape::List,
    DispatchShape::Array,
    DispatchShape::Hash,
    DispatchShape::Str,
    DispatchShape::Int,
    DispatchShape::Num,
    DispatchShape::Rat,
    DispatchShape::FatRat,
    DispatchShape::Complex,
];

/// `(shape, method, arity) -> row`, resolved along each shape's MRO, plus the
/// arities each method name has rows for.
struct Table {
    rows: FxHashMap<(DispatchShape, Symbol, u8), RowId>,
    /// Every row once, indexed by [`RowId`].
    all: Vec<&'static MethodRow>,
    /// Per `Symbol` id, one bit per arity some row with that name takes (bit
    /// `a` for `a` arguments). Most calls are to methods with no row, or with
    /// a row for another arity (`$s.index($n, $from)` beside the one-needle
    /// row); testing a bit answers those without hashing.
    arities: Vec<u8>,
    /// Per `Symbol` id, one bit per [`DispatchShape`] some row with that name
    /// is resolved for. A name with rows for other receivers (`Int` has rows
    /// on the numeric types, none on `Str`) is refused for this one by a bit
    /// test too, without the hash lookup or a call site's memo.
    shapes: Vec<u16>,
}

impl Table {
    /// Whether some row with `method`'s name is resolved for `shape`.
    // Cost: O(1), a bit test.
    #[inline]
    fn has_shape(&self, method: Symbol, shape: DispatchShape) -> bool {
        self.shapes
            .get(method.id() as usize)
            .is_some_and(|bits| bits & (1 << (shape as u16)) != 0)
    }

    /// Whether some row is named `method` and takes `arity` arguments.
    // Cost: O(1), a bit test.
    #[inline]
    fn has_name(&self, method: Symbol, arity: usize) -> bool {
        arity < 8
            && self
                .arities
                .get(method.id() as usize)
                .is_some_and(|bits| bits & (1 << arity) != 0)
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

/// Built on the first lookup, not at `Interpreter` construction: the cost is
/// one pass over the rows per shape (a few dozen inserts today).
fn table() -> &'static Table {
    static TABLE: OnceLock<Table> = OnceLock::new();
    TABLE.get_or_init(|| {
        let all: Vec<&'static MethodRow> = all_rows().collect();
        let mut rows = FxHashMap::default();
        let mut arities = Vec::new();
        let mut shapes = Vec::new();
        for shape in SHAPES {
            let Some(mro) = crate::builtin_types::catalog::builtin_type_mro_syms(shape_type(shape))
            else {
                continue;
            };
            for owner in mro.iter() {
                for (idx, row) in all.iter().enumerate() {
                    // A row past `u16::MAX` stays unreachable through the
                    // table; with a few dozen rows it does not exist.
                    if row.owner == owner.as_str()
                        && let Ok(id) = u16::try_from(idx)
                    {
                        let name = Symbol::intern(row.name);
                        let id = RowId(id);
                        rows.entry((shape, name, row.arity)).or_insert(id);
                        let id = name.id() as usize;
                        if arities.len() <= id {
                            arities.resize(id + 1, 0u8);
                        }
                        if row.arity < 8 {
                            arities[id] |= 1 << row.arity;
                        }
                        if shapes.len() <= id {
                            shapes.resize(id + 1, 0u16);
                        }
                        shapes[id] |= 1 << (shape as u16);
                    }
                }
            }
        }
        Table {
            rows,
            all,
            arities,
            shapes,
        }
    })
}

/// Whether any row is named `method` and takes `arity` arguments: the test
/// every lookup makes first.
// Cost: O(1), a bit test.
#[inline]
pub(crate) fn names_a_row(method: Symbol, arity: usize) -> bool {
    table().has_name(method, arity)
}

/// Whether a receiver of `shape` may have a row for `method`: the second bit
/// test, once the receiver's shape is known. `false` is definite.
// Cost: O(1), a bit test.
#[inline]
pub(crate) fn shape_has_row(shape: DispatchShape, method: Symbol) -> bool {
    table().has_shape(method, shape)
}

/// The row a plain receiver of `shape` dispatches `method` to when called
/// with `arity` positional arguments, if the table has one.
// Cost: O(1), a bit test and one hash lookup.
#[inline]
pub(crate) fn resolve(shape: DispatchShape, method: Symbol, arity: usize) -> Option<RowId> {
    let table = table();
    if !table.has_name(method, arity) || !table.has_shape(method, shape) {
        return None;
    }
    let arity = u8::try_from(arity).ok()?;
    table.rows.get(&(shape, method, arity)).copied()
}

/// The row `id` names.
// Cost: O(1).
#[inline]
pub(crate) fn row(id: RowId) -> &'static MethodRow {
    table().all[usize::from(id.0)]
}

/// The row a plain receiver of `shape` dispatches `method` to, if any (the
/// lookup [`try_dispatch`] makes, for tests).
#[cfg(test)]
fn lookup(shape: DispatchShape, method: Symbol, arity: u8) -> Option<&'static MethodRow> {
    let table = table();
    if !table.has_name(method, usize::from(arity)) {
        return None;
    }
    table.rows.get(&(shape, method, arity)).map(|id| row(*id))
}

#[cfg(test)]
mod tests;
