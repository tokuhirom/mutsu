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

mod basic;
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
    /// Pure data assembly the VM's native `.new` fast path may run directly
    /// (`try_native_builtin_construct`): the entry touches no env, registry or
    /// user code beyond what `Interpreter` state a plain `dispatch_new` also does.
    pub(super) fast: bool,
}

/// Every native constructor by class name, sorted for [`lookup`].
static CTORS: &[(&str, CtorFn, bool, bool)] = &[
    ("Array", Interpreter::ctor_array, false, false),
    ("Backtrace", Interpreter::ctor_backtrace, false, false),
    ("Bag", Interpreter::ctor_bag, false, false),
    ("BagHash", Interpreter::ctor_bag, false, false),
    ("Blob", Interpreter::ctor_buf, false, true),
    ("Blob[uint8]", Interpreter::ctor_buf, false, true),
    ("Buf", Interpreter::ctor_buf, false, true),
    ("Buf[uint8]", Interpreter::ctor_buf, false, true),
    ("CArray", Interpreter::ctor_array, false, false),
    ("Cancellation", Interpreter::ctor_cancellation, false, true),
    ("Capture", Interpreter::ctor_capture, false, true),
    ("Channel", Interpreter::ctor_channel, false, true),
    ("CompUnit", Interpreter::ctor_compunit, false, false),
    ("CompUnit::DependencySpecification", Interpreter::ctor_compunit_dependencyspecification, false, false),
    ("CompUnit::Repository::FileSystem", Interpreter::ctor_compunit_repository_filesystem, false, false),
    ("CompUnit::Repository::Installation", Interpreter::ctor_compunit_repository_installation, false, false),
    ("Complex", Interpreter::ctor_complex, false, true),
    ("ComplexStr", Interpreter::ctor_allomorph, false, true),
    ("CurrentThreadScheduler", Interpreter::ctor_threadpoolscheduler, false, true),
    ("Date", Interpreter::ctor_date, false, false),
    ("DateTime", Interpreter::ctor_datetime, false, false),
    ("Distribution::Hash", Interpreter::ctor_distribution_hash, false, false),
    ("Distribution::Path", Interpreter::ctor_distribution_path, false, false),
    ("Duration", Interpreter::ctor_duration, false, true),
    ("Encoding::Decoder::Builtin", Interpreter::ctor_encoding_decoder_builtin, false, false),
    ("Failure", Interpreter::ctor_failure, false, false),
    ("FakeScheduler", Interpreter::ctor_fakescheduler, false, true),
    ("FatRat", Interpreter::ctor_fatrat, false, true),
    ("Hash", Interpreter::ctor_hash, false, false),
    ("HyperWhatever", Interpreter::ctor_hyperwhatever, false, false),
    ("IO::CatHandle", Interpreter::ctor_io_cathandle, true, false),
    ("IO::Socket::INET", Interpreter::ctor_io_socket_inet, false, false),
    ("Instant", Interpreter::ctor_hyperwhatever, false, false),
    ("Int", Interpreter::ctor_int, false, true),
    ("IntStr", Interpreter::ctor_allomorph, false, true),
    ("IterationBuffer", Interpreter::ctor_iterationbuffer, false, true),
    ("Junction", Interpreter::ctor_junction, false, false),
    ("List", Interpreter::ctor_array, false, false),
    ("Lock", Interpreter::ctor_lock, false, true),
    ("Lock::Async", Interpreter::ctor_lock, false, true),
    ("Lock::Soft", Interpreter::ctor_lock, false, true),
    ("Map", Interpreter::ctor_hash, false, false),
    ("Match", Interpreter::ctor_match, false, true),
    ("Mix", Interpreter::ctor_mix, false, false),
    ("MixHash", Interpreter::ctor_mix, false, false),
    ("Num", Interpreter::ctor_num, false, true),
    ("NumStr", Interpreter::ctor_allomorph, false, true),
    ("ObjAt", Interpreter::ctor_objat, false, true),
    ("Pair", Interpreter::ctor_pair, false, true),
    ("Parameter", Interpreter::ctor_parameter, false, false),
    ("Positional", Interpreter::ctor_array, false, false),
    ("Proc::Async", Interpreter::ctor_proc_async, false, true),
    ("Promise", Interpreter::ctor_promise, false, true),
    ("Proxy", Interpreter::ctor_proxy, false, true),
    ("PseudoStash", Interpreter::ctor_stash, false, true),
    ("Rat", Interpreter::ctor_rat, false, true),
    ("RatStr", Interpreter::ctor_allomorph, false, true),
    ("Seq", Interpreter::ctor_seq, false, false),
    ("Set", Interpreter::ctor_set, false, false),
    ("SetHash", Interpreter::ctor_set, false, false),
    ("Signature", Interpreter::ctor_signature, false, false),
    ("Slip", Interpreter::ctor_slip, false, true),
    ("Stash", Interpreter::ctor_stash, false, true),
    ("Str", Interpreter::ctor_str, false, true),
    ("StrDistance", Interpreter::ctor_strdistance, false, true),
    ("Supplier", Interpreter::ctor_supplier, false, true),
    ("Supplier::Preserving", Interpreter::ctor_supplier, false, true),
    ("Supply", Interpreter::ctor_supply, false, false),
    ("Tap", Interpreter::ctor_threadpoolscheduler, false, true),
    ("Thread", Interpreter::ctor_thread, false, false),
    ("ThreadPoolScheduler", Interpreter::ctor_threadpoolscheduler, false, true),
    ("Uni", Interpreter::ctor_uni, false, true),
    ("ValueObjAt", Interpreter::ctor_objat, false, true),
    ("Version", Interpreter::ctor_version, false, true),
    ("Whatever", Interpreter::ctor_whatever, false, false),
    ("array", Interpreter::ctor_array, false, false),
    ("blob16", Interpreter::ctor_buf, false, true),
    ("blob32", Interpreter::ctor_buf, false, true),
    ("blob64", Interpreter::ctor_buf, false, true),
    ("blob8", Interpreter::ctor_buf, false, true),
    ("buf16", Interpreter::ctor_buf, false, true),
    ("buf32", Interpreter::ctor_buf, false, true),
    ("buf64", Interpreter::ctor_buf, false, true),
    ("buf8", Interpreter::ctor_buf, false, true),
    ("utf16", Interpreter::ctor_utf8, false, true),
    ("utf8", Interpreter::ctor_utf8, false, true),
];

/// The native constructor of `class_name`, a class name without type arguments.
// Cost: O(log c), c = the number of constructors.
pub(super) fn lookup(class_name: &str) -> Option<NativeCtor> {
    let i = CTORS.binary_search_by_key(&class_name, |entry| entry.0).ok()?;
    let (_, run, unless_user_new, fast) = CTORS[i];
    Some(NativeCtor {
        run,
        unless_user_new,
        fast,
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
