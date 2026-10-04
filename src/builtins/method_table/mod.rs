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
//! `(shape, method symbol)` up in a map built once per process by walking each
//! shape's MRO (`builtin_types::catalog`) most-derived first. A miss, or a
//! call whose positional count is not the row's arity, returns `None` and the
//! call takes the cascades exactly as before; that fallback is what lets the
//! families migrate one at a time (ADR-11276 §6).
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

mod complex;
mod int;
mod list;
mod map;
mod num;
mod rational;
pub(crate) mod real;
pub(crate) mod str;

use crate::symbol::Symbol;
use crate::value::{DispatchShape, RuntimeError, Value};
use rustc_hash::FxHashMap;
use std::sync::OnceLock;

/// A built-in method's implementation.
///
/// Only the kind the rows so far need exists yet. ADR-11276 §2 adds an
/// interpreter-taking kind and a receiver-writing kind with the first rows
/// that need them.
#[derive(Clone, Copy)]
pub(crate) enum Handler {
    /// Needs no interpreter. `args` are the positional arguments, exactly
    /// [`MethodRow::arity`] of them.
    Pure(fn(&Value, &[Value]) -> Result<Value, RuntimeError>),
}

/// One built-in method: see the module docs.
#[derive(Clone, Copy)]
pub(crate) struct MethodRow {
    /// The type Rakudo declares the method on (a key of its `^method_table`).
    pub(crate) owner: &'static str,
    pub(crate) name: &'static str,
    /// The number of positional arguments the row takes.
    pub(crate) arity: u8,
    pub(crate) handler: Handler,
}

/// Every family's rows. A family module owns the rows of one declaring type.
static FAMILIES: &[&[MethodRow]] = &[
    list::ROWS,
    map::ROWS,
    str::ROWS,
    str::STR_TEXT_ROWS,
    str::COOL_TEXT_ROWS,
    int::ROWS,
    num::ROWS,
    rational::RAT_ROWS,
    rational::FAT_RAT_ROWS,
    complex::ROWS,
    real::INT_ROWS,
    real::NUM_ROWS,
    real::RAT_ROWS,
    real::FAT_RAT_ROWS,
    real::COMPLEX_ROWS,
];

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

/// `(shape, method) -> row`, resolved along each shape's MRO, plus the set of
/// method names any row has.
struct Table {
    rows: FxHashMap<(DispatchShape, Symbol), RowId>,
    /// Every row once, indexed by [`RowId`].
    all: Vec<&'static MethodRow>,
    /// One bit per `Symbol` id that names some row. Most calls are to methods
    /// with no row yet; testing a bit answers those without hashing.
    names: Vec<u64>,
}

impl Table {
    fn has_name(&self, method: Symbol) -> bool {
        let id = method.id() as usize;
        self.names
            .get(id / 64)
            .is_some_and(|word| word & (1 << (id % 64)) != 0)
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
        let all: Vec<&'static MethodRow> = FAMILIES.iter().flat_map(|rows| rows.iter()).collect();
        let mut rows = FxHashMap::default();
        let mut names = Vec::new();
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
                        rows.entry((shape, name)).or_insert(id);
                        let id = name.id() as usize;
                        if names.len() <= id / 64 {
                            names.resize(id / 64 + 1, 0u64);
                        }
                        names[id / 64] |= 1 << (id % 64);
                    }
                }
            }
        }
        Table { rows, all, names }
    })
}

/// Whether any row is named `method`: the test every lookup makes first.
// Cost: O(1), a bit test.
#[inline]
pub(crate) fn names_a_row(method: Symbol) -> bool {
    table().has_name(method)
}

/// The row a plain receiver of `shape` dispatches `method` to when called
/// with `arity` positional arguments, if the table has one.
// Cost: O(1), a bit test and one hash lookup.
#[inline]
pub(crate) fn resolve(shape: DispatchShape, method: Symbol, arity: usize) -> Option<RowId> {
    let table = table();
    if !table.has_name(method) {
        return None;
    }
    let id = *table.rows.get(&(shape, method))?;
    (usize::from(table.all[usize::from(id.0)].arity) == arity).then_some(id)
}

/// The row `id` names.
// Cost: O(1).
#[inline]
pub(crate) fn row(id: RowId) -> &'static MethodRow {
    table().all[usize::from(id.0)]
}

/// Run row `id`'s handler. `args` must have the row's arity, and `target`
/// the shape the row was resolved for.
// Cost: O(1) plus the handler's own cost.
#[inline]
pub(crate) fn invoke(id: RowId, target: &Value, args: &[Value]) -> Result<Value, RuntimeError> {
    match row(id).handler {
        Handler::Pure(f) => f(target, args),
    }
}

/// The row a plain receiver of `shape` dispatches `method` to, if any (the
/// lookup [`try_dispatch`] makes, for tests).
#[cfg(test)]
fn lookup(shape: DispatchShape, method: Symbol) -> Option<&'static MethodRow> {
    let table = table();
    if !table.has_name(method) {
        return None;
    }
    table.rows.get(&(shape, method)).map(|id| row(*id))
}

/// Answer a built-in method call from its row, or `None` to take the cascades.
///
/// `None` is always safe: the call then walks the cascades exactly as it did
/// before the table existed.
// Cost: O(1) to find the row (a bit test, a tag probe and one hash lookup),
// plus the handler's own cost.
#[inline]
pub(crate) fn try_dispatch(
    target: &Value,
    method: Symbol,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    let result = answer(target, method, args)?;
    debug_assert_matches_full_path(target, method, args, &result);
    Some(result)
}

/// [`try_dispatch`] without the debug cross-check: what the cascades' own
/// entry (`native_method_0arg`) asks first.
// Cost: O(1) to find the row (a bit test, a tag probe and one hash lookup),
// plus the handler's own cost.
#[inline]
pub(crate) fn answer(
    target: &Value,
    method: Symbol,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    if !table().has_name(method) {
        return None;
    }
    let shape = target.dispatch_shape()?;
    let id = resolve(shape, method, args.len())?;
    Some(invoke(id, target, args))
}

/// In debug builds, re-answer a table hit through the cascades and assert
/// the two agree.
///
/// This is the maintenance net for the table. A row is admitted by an argument
/// about which skipped probes could claim the call, and a later commit adding
/// a probe has no way of knowing it invalidated one; running both paths over
/// the whole TAP suite turns that silent divergence into a failing assertion.
/// A cascade that declines agrees: a migrated method has no arm left there.
/// Sound to run twice only because every pure row is side-effect free.
fn debug_assert_matches_full_path(
    target: &Value,
    method: Symbol,
    args: &[Value],
    fast: &Result<Value, RuntimeError>,
) {
    #[cfg(debug_assertions)]
    {
        let slow = match args {
            [] => super::methods_0arg::native_method_0arg_cascade(target, method),
            [a] => super::native_method_1arg(target, method, a),
            [a, b] => super::native_method_2arg(target, method, a, b),
            _ => return,
        };
        let render = |r: Option<&Result<Value, RuntimeError>>| match r {
            None => "<declined>".to_string(),
            Some(Ok(v)) => format!("ok:{}", crate::runtime::gist_value(v)),
            Some(Err(e)) => format!("err:{}", e.message),
        };
        let Some(slow) = slow else {
            return;
        };
        debug_assert_eq!(
            render(Some(fast)),
            render(Some(&slow)),
            "method_table row disagrees with the full path for .{} on a {:?} \
             receiver -- a probe the table skips now claims this call",
            method.as_str(),
            target.dispatch_shape(),
        );
    }
    #[cfg(not(debug_assertions))]
    {
        let _ = (target, method, args, fast);
    }
}

#[cfg(test)]
#[path = "tests.rs"]
mod tests;
