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
//! `dispatch::try_dispatch_in` (the VM's native entry and
//! `call_method_with_values`), the call-site lane and the cascades' own prologue
//! (`answer`) all go through one guard step. It decodes the receiver as a
//! [`Receiver`]: an instance of a [`DispatchShape`](crate::value::DispatchShape)
//! (a tag probe that refuses everything the receiver-state checks in
//! `vm_native_dispatch::try_native_method_raw` exist for: user instances, mixins,
//! containers, lazy and shaped values), or the type object of a built-in type. It
//! looks `(receiver, method symbol, arity)` up in a map built once per process by
//! walking each shape's MRO (`builtin_types::catalog`) most-derived first; a shape
//! added after the first nine is closed to ancestor rows (see
//! `value::dispatch_shape`). The call's arguments are split into positional and
//! named ones (a string-keyed `Pair` is a named argument, ADR-0021), the row is
//! found by the positional arity, and each argument is admitted by the row's
//! [`RowFlags`]: a plain scalar by default, any plain argument for `ANY_ARGS`.
//! Junctions, Failures, lazy Seqs and the like need probes the table skips. A
//! miss, a name no row binds, an argument not admitted, or a
//! [`Handler::Narrow`] row that does not bind the arguments returns `None`, and
//! the call takes the cascades exactly as before; that fallback is what lets the
//! families migrate one at a time (ADR-11276 §6).
//!
//! # Adding a row
//!
//! Put the handler and its row in the family module of the type Rakudo
//! declares the method on, in the directory of the slice group that owns the
//! type (`scalars`, `collections`, `instances`, ...), and make any cascade arm that
//! answers the same method call the handler. A pair may be added only when no
//! probe the table skips can claim it for a plain receiver of that shape — the
//! probes are listed in `try_native_method_raw`, `native_method_0arg`'s prologue
//! and `dispatch_core`'s prologue. In debug builds every table hit of a pure row
//! is re-answered through the full pure path and the two must agree
//! (`dispatch::debug_assert_matches_full_path`); CI's `debug-tap` job runs that
//! over the whole TAP suite. `rows_are_declared_by_rakudo` checks each row's owner.

pub(crate) mod allomorph;
mod collections;
mod ctors_mop;
mod dispatch;
pub(crate) mod index_arg;
mod instances;
mod io_concurrency;
mod mutating;
mod place;
mod row;
pub(crate) mod scalars;
mod table;
mod table_const;
mod unadmitted;

// The family modules keep their historical paths
// (`method_table::str::tclc`), whichever group directory holds them.
pub(crate) use collections::iteration_buffer::owners_of as iteration_buffer_owners_of;
pub(crate) use collections::{
    any_collection, clone as collection_clone, lazy, list, list_aggregate, map, positional,
    quanthash, range, render as collection_render, render_names as collection_render_names,
    rotate as collection_rotate, subscript, terminals,
};
pub(crate) use ctors_mop::attribute::owners_of as attribute_owners_of;
pub(crate) use instances::collation::owners_of as collation_owners_of;
pub(crate) use instances::compunit::owners_of as compunit_owners_of;
pub(crate) use instances::enumeration::owners_of as enumeration_owners_of;
#[cfg(test)]
pub(crate) use instances::instant::sample as instances_sample;
pub(crate) use instances::iterator::owners_of as iterator_owners_of;
pub(crate) use instances::rakuast_node::{origin_value, owners_of as rakuast_owners_of};
pub(crate) use instances::routine::owners_of as routine_owners_of;
pub(crate) use instances::supply::owners_of as supply_owners_of;
pub(crate) use instances::{backtrace, code, exception, failure, regex_match, signature, supply};
pub(crate) use io_concurrency::cathandle::owners_of as cathandle_owners_of;
pub(crate) use io_concurrency::class_methods::{
    early_owners_of as early_class_method_owners_of, owners_of as class_method_owners_of,
};
pub(crate) use io_concurrency::proc_async::owners_of as proc_async_owners_of;
pub(crate) use io_concurrency::promise_channel;
pub(crate) use io_concurrency::promise_channel::{
    CHANNEL_LIST_COERCIONS, owners_of as promise_channel_owners_of,
    receiver_of as promise_channel_receiver_of,
};
pub(crate) use io_concurrency::supplier_scheduler;
pub(crate) use io_concurrency::supplier_scheduler::owners_of as supplier_scheduler_owners_of;
pub(crate) use scalars::{
    blob, coerce, complex_math, numeric_base, numeric_coerce, numify, real, real_misc, str,
    str_args, str_numeric, str_search, truth, uni,
};

pub(crate) use collections::accepts_open::accepts_by_smartmatch;
pub(crate) use collections::any_subscript::at_pos as any_subscript_at_pos;
pub(crate) use collections::early_entry_defers;
pub(crate) use ctors_mop::{MOP_OWNERS, mop_declares};
pub(crate) use dispatch::{
    admits, answer, answer_as, answer_shapeless, cool_text_for_shapeless, invoke, invoke_base,
    invoke_in, invoke_mro, invoke_mut, invoke_owner, invoke_owner_raw, try_dispatch_in,
};
#[cfg(test)]
pub(crate) use dispatch::{shapeless_row, try_dispatch};
pub(crate) use mutating::buf::by_value as buf_by_value;
pub(crate) use mutating::owners_of as mut_owners_of;
pub(crate) use place::ReceiverPlace;
pub(crate) use row::{Handler, MethodRow, Named, NamedFn, NarrowFn, RowFlags};
#[cfg(test)]
use table::all_rows;
#[cfg(test)]
pub(crate) use table::names_a_mut_row;
pub(crate) use table::{
    Receiver, RowId, base_row, max_row_arity, names_a_row, owner_declares, owner_row,
    owner_rows_at, resolve, row, shape_has_row,
};
pub(crate) use unadmitted::invoke_unadmitted;

#[cfg(test)]
mod tests;
