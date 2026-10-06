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
mod table;

// The family modules keep their historical paths
// (`method_table::str::tclc`), whichever group directory holds them.
pub(crate) use collections::{
    any_collection, list, list_aggregate, list_transform, map, pair, positional, range,
};
pub(crate) use instances::temporal;
pub(crate) use scalars::{
    coerce, complex, real, str, str_iter, str_search, succ_pred, truth, version,
};

#[cfg(test)]
pub(crate) use dispatch::try_dispatch;
pub(crate) use dispatch::{admits, answer, invoke, invoke_in, try_dispatch_in};
pub(crate) use row::{Handler, MethodRow, Named, RowFlags};
#[cfg(test)]
use table::all_rows;
use table::table;
pub(crate) use table::{Receiver, RowId, names_a_row, resolve, row, shape_has_row};

#[cfg(test)]
mod tests;
