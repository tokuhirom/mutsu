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

mod complex;
mod int;
mod list;
mod map;
mod num;
mod rational;
pub(crate) mod real;
pub(crate) mod str;
pub(crate) mod str_search;

use crate::symbol::Symbol;
use crate::value::{DispatchShape, RuntimeError, Value};
use rustc_hash::FxHashMap;
use std::sync::OnceLock;

/// A [`Handler::Narrow`] implementation.
pub(crate) type NarrowFn = fn(&Value, &[Value]) -> Option<Result<Value, RuntimeError>>;

/// A built-in method's implementation.
///
/// Only the kind the rows so far need exists yet. ADR-11276 §2 adds an
/// interpreter-taking kind and a receiver-writing kind with the first rows
/// that need them.
#[derive(Clone, Copy)]
pub(crate) enum Handler {
    /// Needs no interpreter. `args` are the positional arguments, exactly
    /// [`MethodRow::arity`] of them, each a plain scalar ([`answer`]).
    Pure(fn(&Value, &[Value]) -> Result<Value, RuntimeError>),
    /// A [`Self::Pure`] handler whose row binds only some argument values:
    /// `None` means these arguments are outside the row's signature, and the
    /// call takes the cascades (the way a multi candidate fails to bind).
    Narrow(NarrowFn),
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
    str_search::STR_ROWS,
    str_search::COOL_ROWS,
    str_search::STR_SUBSTR_2,
    str_search::COOL_SUBSTR_2,
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
}

impl Table {
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
        let all: Vec<&'static MethodRow> = FAMILIES.iter().flat_map(|rows| rows.iter()).collect();
        let mut rows = FxHashMap::default();
        let mut arities = Vec::new();
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
                    }
                }
            }
        }
        Table { rows, all, arities }
    })
}

/// Whether any row is named `method` and takes `arity` arguments: the test
/// every lookup makes first.
// Cost: O(1), a bit test.
#[inline]
pub(crate) fn names_a_row(method: Symbol, arity: usize) -> bool {
    table().has_name(method, arity)
}

/// The row a plain receiver of `shape` dispatches `method` to when called
/// with `arity` positional arguments, if the table has one.
// Cost: O(1), a bit test and one hash lookup.
#[inline]
pub(crate) fn resolve(shape: DispatchShape, method: Symbol, arity: usize) -> Option<RowId> {
    let table = table();
    if !table.has_name(method, arity) {
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

/// Run row `id`'s handler, or `None` when a [`Handler::Narrow`] row does not
/// bind these arguments. `args` must have the row's arity and be plain
/// scalars ([`plain_args`]), and `target` must have the shape the row was
/// resolved for.
// Cost: O(1) plus the handler's own cost.
#[inline]
pub(crate) fn invoke(
    id: RowId,
    target: &Value,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    match row(id).handler {
        Handler::Pure(f) => Some(f(target, args)),
        Handler::Narrow(f) => f(target, args),
    }
}

/// Whether every argument is a plain scalar a row may be handed: a `Str` or
/// a number. Named arguments arrive as `Pair`s, and a `Junction` (which must
/// autothread), a `Failure`, a lazy `Seq`, a `Regex`, a list or a type object
/// each needs a probe the table skips, so a call carrying any of them takes
/// the cascades.
// Cost: O(a), a = arguments (one tag probe each).
#[inline]
pub(crate) fn plain_args(args: &[Value]) -> bool {
    args.iter().all(|arg| {
        matches!(
            arg.dispatch_shape(),
            Some(
                DispatchShape::Str
                    | DispatchShape::Int
                    | DispatchShape::Num
                    | DispatchShape::Rat
                    | DispatchShape::FatRat
            )
        )
    })
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
    if !table().has_name(method, args.len()) {
        return None;
    }
    let shape = target.dispatch_shape()?;
    let id = resolve(shape, method, args.len())?;
    // After the lookup: a call that misses never pays for the argument scan.
    if !plain_args(args) {
        return None;
    }
    invoke(id, target, args)
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
