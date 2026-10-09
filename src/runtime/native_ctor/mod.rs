//! Native constructors by type (ADR-11276 slice 4, #12423).
//!
//! Rakudo resolves `Hash.new`, `Set.new`, `Version.new`, `Promise.new`, ... to
//! `Mu.new` or to a method each class declares itself; mutsu builds those values
//! natively. The constructors used to be three parallel type-name ladders (the
//! `match` in `dispatch_new_unallocated`, the `cn == ...` chain behind the VM's
//! native fast path, and a few arms in `methods_dispatch_new.rs`). This table is
//! the first of them as data: one entry per class name, looked up by
//! [`lookup`], so a constructor is added or changed in one place.
//!
//! It is a table of *constructors*, not method rows: the method table keys a row
//! by the type Rakudo declares the method on, and most of these types declare no
//! `new` of their own (`rows_are_declared_by_rakudo` would reject them).

use crate::runtime::*;
use crate::symbol::Symbol;

mod collections;
mod concurrency;
mod distribution;
mod io_temporal;
mod meta;
mod numeric;

/// What a constructor needs to know about the call besides its arguments.
pub(super) struct CtorCall<'a> {
    /// The class being constructed, as written (`Supplier::Preserving`, `Buf[uint8]`).
    pub(super) class_name: Symbol,
    /// The registered class whose methods answer for it: the class itself when
    /// registered, else its base name.
    pub(super) class_key: &'a str,
    /// `class_name` without its type arguments (`Array[Int]` -> `Array`).
    pub(super) base_class_name: &'a str,
    /// The parsed type arguments of a parametric `class_name`.
    pub(super) type_args: &'a Option<Vec<String>>,
}

type CtorFn = fn(&mut Interpreter, &CtorCall<'_>, Vec<Value>) -> Result<Value, RuntimeError>;

/// One native constructor.
pub(super) struct NativeCtor {
    run: CtorFn,
    /// A user `new` of the class (found in `class_key`) answers instead.
    unless_user_new: bool,
}

/// Every native constructor by class name, sorted for [`lookup`].
static CTORS: &[(&str, CtorFn, bool)] = &[
    ("Array", Interpreter::ctor_array, false),
    ("Backtrace", Interpreter::ctor_backtrace, false),
    ("Bag", Interpreter::ctor_bag, false),
    ("BagHash", Interpreter::ctor_bag, false),
    ("Blob", Interpreter::ctor_buf, false),
    ("Blob[uint8]", Interpreter::ctor_buf, false),
    ("Buf", Interpreter::ctor_buf, false),
    ("Buf[uint8]", Interpreter::ctor_buf, false),
    ("CArray", Interpreter::ctor_array, false),
    ("Cancellation", Interpreter::ctor_cancellation, false),
    ("Channel", Interpreter::ctor_channel, false),
    ("CompUnit", Interpreter::ctor_compunit, false),
    ("CompUnit::DependencySpecification", Interpreter::ctor_compunit_dependencyspecification, false),
    ("CompUnit::Repository::FileSystem", Interpreter::ctor_compunit_repository_filesystem, false),
    ("CompUnit::Repository::Installation", Interpreter::ctor_compunit_repository_installation, false),
    ("Complex", Interpreter::ctor_complex, false),
    ("CurrentThreadScheduler", Interpreter::ctor_threadpoolscheduler, false),
    ("Date", Interpreter::ctor_date, false),
    ("DateTime", Interpreter::ctor_datetime, false),
    ("Distribution::Hash", Interpreter::ctor_distribution_hash, false),
    ("Distribution::Path", Interpreter::ctor_distribution_path, false),
    ("Duration", Interpreter::ctor_duration, false),
    ("Encoding::Decoder::Builtin", Interpreter::ctor_encoding_decoder_builtin, false),
    ("FakeScheduler", Interpreter::ctor_fakescheduler, false),
    ("FatRat", Interpreter::ctor_fatrat, false),
    ("Hash", Interpreter::ctor_hash, false),
    ("HyperWhatever", Interpreter::ctor_hyperwhatever, false),
    ("IO::CatHandle", Interpreter::ctor_io_cathandle, true),
    ("IO::Socket::INET", Interpreter::ctor_io_socket_inet, false),
    ("Instant", Interpreter::ctor_hyperwhatever, false),
    ("IterationBuffer", Interpreter::ctor_iterationbuffer, false),
    ("Junction", Interpreter::ctor_junction, false),
    ("List", Interpreter::ctor_array, false),
    ("Lock", Interpreter::ctor_lock, false),
    ("Lock::Async", Interpreter::ctor_lock, false),
    ("Lock::Soft", Interpreter::ctor_lock, false),
    ("Map", Interpreter::ctor_hash, false),
    ("Match", Interpreter::ctor_match, false),
    ("Mix", Interpreter::ctor_mix, false),
    ("MixHash", Interpreter::ctor_mix, false),
    ("Pair", Interpreter::ctor_pair, false),
    ("Parameter", Interpreter::ctor_parameter, false),
    ("Positional", Interpreter::ctor_array, false),
    ("Proc::Async", Interpreter::ctor_proc_async, false),
    ("Promise", Interpreter::ctor_promise, false),
    ("Proxy", Interpreter::ctor_proxy, false),
    ("PseudoStash", Interpreter::ctor_stash, false),
    ("Rat", Interpreter::ctor_rat, false),
    ("Seq", Interpreter::ctor_seq, false),
    ("Set", Interpreter::ctor_set, false),
    ("SetHash", Interpreter::ctor_set, false),
    ("Signature", Interpreter::ctor_signature, false),
    ("Slip", Interpreter::ctor_slip, false),
    ("Stash", Interpreter::ctor_stash, false),
    ("StrDistance", Interpreter::ctor_strdistance, false),
    ("Supplier", Interpreter::ctor_supplier, false),
    ("Supplier::Preserving", Interpreter::ctor_supplier, false),
    ("Supply", Interpreter::ctor_supply, false),
    ("Tap", Interpreter::ctor_threadpoolscheduler, false),
    ("Thread", Interpreter::ctor_thread, false),
    ("ThreadPoolScheduler", Interpreter::ctor_threadpoolscheduler, false),
    ("Uni", Interpreter::ctor_uni, false),
    ("Version", Interpreter::ctor_version, false),
    ("Whatever", Interpreter::ctor_hyperwhatever, false),
    ("array", Interpreter::ctor_array, false),
    ("blob16", Interpreter::ctor_buf, false),
    ("blob32", Interpreter::ctor_buf, false),
    ("blob64", Interpreter::ctor_buf, false),
    ("blob8", Interpreter::ctor_buf, false),
    ("buf16", Interpreter::ctor_buf, false),
    ("buf32", Interpreter::ctor_buf, false),
    ("buf64", Interpreter::ctor_buf, false),
    ("buf8", Interpreter::ctor_buf, false),
    ("utf16", Interpreter::ctor_utf8, false),
    ("utf8", Interpreter::ctor_utf8, false),
];

/// The native constructor of `class_name`, a class name without type arguments.
// Cost: O(log c), c = the number of constructors.
pub(super) fn lookup(class_name: &str) -> Option<NativeCtor> {
    let i = CTORS.binary_search_by_key(&class_name, |entry| entry.0).ok()?;
    let (_, run, unless_user_new) = CTORS[i];
    Some(NativeCtor {
        run,
        unless_user_new,
    })
}

impl Interpreter {
    /// Run `ctor` for `call`, or `None` when a user `new` of the class takes
    /// over (`IO::CatHandle`).
    // Cost: O(1) plus the constructor's own cost.
    pub(super) fn run_native_ctor(
        &mut self,
        ctor: &NativeCtor,
        call: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        if ctor.unless_user_new && self.has_user_method(call.class_key, "new") {
            return None;
        }
        Some((ctor.run)(self, call, args))
    }
}

#[cfg(test)]
mod tests {
    use super::CTORS;

    /// `lookup` binary-searches by name.
    #[test]
    fn the_table_is_sorted_and_has_no_duplicates() {
        for pair in CTORS.windows(2) {
            assert!(pair[0].0 < pair[1].0, "{} before {}", pair[0].0, pair[1].0);
        }
    }
}
