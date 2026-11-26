//! The core rows of the static class catalogue (ADR-19206 slice 2): the
//! built-in classes whose `ClassDef` is a literal with no attributes, moved
//! verbatim out of `build_builtin_registry`. A class is built by
//! [`build_core_class`] on its first lookup instead of at every start-up.
//!
//! `CORE_CLASS_ROWS` lists the names, one `name<TAB><TAB>` row each, sorted
//! by name (the catalogue checks this at compile time); [`build_core_class`]
//! builds row `index`. Classes with attributes stay in `build_builtin_registry`:
//! their accessor rows are written into the method tables there.

use super::ClassDef;

pub(super) const CORE_CLASS_ROWS: &str = concat!(
    "Any\t\t\n",
    "Backtrace\t\t\n",
    "Blob\t\t\n",
    "Buf\t\t\n",
    "Cancellation\t\t\n",
    "Channel\t\t\n",
    "Collation\t\t\n",
    "CompUnit::Repository::FileSystem\t\t\n",
    "CurrentThreadScheduler\t\t\n",
    "Encoding::Decoder::Builtin\t\t\n",
    "Encoding::Encoder::Builtin\t\t\n",
    "Encoding::Registry\t\t\n",
    "Exception\t\t\n",
    "FakeScheduler\t\t\n",
    "IO::Path\t\t\n",
    "IO::Pipe\t\t\n",
    "IO::Socket::Async\t\t\n",
    "IO::Socket::Async::ListenSocket\t\t\n",
    "IO::Socket::Async::Listener\t\t\n",
    "IO::Socket::INET\t\t\n",
    "IterationBuffer\t\t\n",
    "Lock\t\t\n",
    "Lock::Async\t\t\n",
    "Lock::ConditionVariable\t\t\n",
    "Lock::Soft\t\t\n",
    "Mu\t\t\n",
    "Proc::Async\t\t\n",
    "Promise\t\t\n",
    "Promise::Vow\t\t\n",
    "Scheduler\t\t\n",
    "Semaphore\t\t\n",
    "Supplier\t\t\n",
    "Supplier::Preserving\t\t\n",
    "Supply\t\t\n",
    "Tap\t\t\n",
    "Thread\t\t\n",
    "ThreadPoolScheduler\t\t\n",
    "X::AdHoc\t\t\n",
    "X::Coerce::Impossible\t\t\n",
    "X::Numeric::Real\t\t\n",
    "X::Parameter\t\t\n",
    "X::Parameter::InvalidConcreteness\t\t\n",
    "X::Supply::Combinator\t\t\n",
    "X::TypeCheck\t\t\n",
    "X::TypeCheck::Argument\t\t\n",
    "X::TypeCheck::Assignment\t\t\n",
    "X::TypeCheck::Binding\t\t\n",
    "X::TypeCheck::Binding::Parameter\t\t\n",
    "X::TypeCheck::Return\t\t\n",
    "__ChannelSupply\t\t\n",
    "__PromiseSupply\t\t\n",
    "__ScheduledTapPump\t\t\n",
    "__SupplyCollector\t\t\n",
    "__SupplyDerive\t\t\n",
    "__SupplyDeriveMulti\t\t\n",
    "__SupplyQuitForwarder\t\t\n",
    "utf16\t\t\n",
    "utf8\t\t\n",
);

/// The class of core row `index` (in [`CORE_CLASS_ROWS`] order), as
/// `build_builtin_registry` used to insert it. The literals live in three
/// files by row range, each under the line budget.
// Cost: O(s), s = the size of the class's literal sets.
pub(super) fn build_core_class(index: usize) -> ClassDef {
    match index {
        0..20 => super::builtin_class_catalogue_core_a::build(index),
        20..40 => super::builtin_class_catalogue_core_b::build(index),
        _ => super::builtin_class_catalogue_core_c::build(index),
    }
}
