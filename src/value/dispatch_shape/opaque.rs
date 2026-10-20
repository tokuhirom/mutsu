//! The opaque instance shapes (ADR-11276 slice 5): an instance of a built-in
//! class whose state lives in the interpreter's tables or in its attributes
//! (a lock, a socket, a scheduler, a compilation unit), and whose rows are its
//! class's own.
//!
//! Such a class used to have no shape: each of its call sites asked a
//! per-class `owners_of` hook for the owners to look its rows up under. A
//! shape makes the table answer it from every entry (the VM's native entry,
//! the by-name and the mutating ones) through the one guard step, and the
//! shape itself names the owners its rows are found under, nearest first.
//!
//! An opaque shape reaches those owners' rows and nothing else: no `Mu`,
//! `Any` or `Cool` row, and no role's. Whatever the class does not declare
//! itself (`gist`, `raku`, `Bool`, `WHICH`, the list methods) is answered as
//! it was before the shape existed, by the interpreter's default object
//! protocol. Its owners need no catalog MRO: they are listed here.

use super::DispatchShape;

/// The type a receiver of the opaque shape `shape` is, and the owners its
/// rows are found under, nearest first; `None` for every other shape.
// Cost: O(1).
pub(super) const fn opaque(
    shape: DispatchShape,
) -> Option<(&'static str, &'static [&'static str])> {
    use DispatchShape as S;
    Some(match shape {
        S::Lock => ("Lock", &["Lock"]),
        S::LockAsync => ("Lock::Async", &["Lock::Async"]),
        S::LockConditionVariable => ("Lock::ConditionVariable", &["Lock::ConditionVariable"]),
        S::Cancellation => ("Cancellation", &["Cancellation"]),
        S::Semaphore => ("Semaphore", &["Semaphore"]),
        S::Thread => ("Thread", &["Thread"]),
        S::Tap => ("Tap", &["Tap"]),
        S::ListenSocket => (
            "IO::Socket::Async::ListenSocket",
            &["IO::Socket::Async::ListenSocket", "Tap"],
        ),
        S::Proc => ("Proc", &["Proc"]),
        S::SocketInet => ("IO::Socket::INET", &["IO::Socket::INET"]),
        S::SocketAsync => ("IO::Socket::Async", &["IO::Socket::Async"]),
        S::CatHandle => ("IO::CatHandle", &["IO::CatHandle"]),
        S::ThreadPoolScheduler => ("ThreadPoolScheduler", &["ThreadPoolScheduler"]),
        S::CurrentThreadScheduler => ("CurrentThreadScheduler", &["CurrentThreadScheduler"]),
        S::EncodingBuiltin => ("Encoding::Builtin", &["Encoding::Builtin"]),
        S::EncodingEncoder => (
            "Encoding::Encoder::Builtin",
            &["Encoding::Encoder::Builtin"],
        ),
        S::EncodingDecoder => (
            "Encoding::Decoder::Builtin",
            &["Encoding::Decoder::Builtin"],
        ),
        S::WalkList => ("WalkList", &["WalkList"]),
        S::WrapHandle => ("Routine::WrapHandle", &["Routine::WrapHandle"]),
        S::HyperConfiguration => ("HyperConfiguration", &["HyperConfiguration"]),
        S::PodDeclarator => ("Pod::Block::Declarator", &["Pod::Block::Declarator"]),
        S::CompUnit => ("CompUnit", &["CompUnit"]),
        S::CompUnitHandle => ("CompUnit::Handle", &["CompUnit::Handle"]),
        S::RepoInstallation => (
            "CompUnit::Repository::Installation",
            &["CompUnit::Repository::Installation"],
        ),
        S::RepoFileSystem => (
            "CompUnit::Repository::FileSystem",
            &["CompUnit::Repository::FileSystem"],
        ),
        S::RepoDistribution => (
            "CompUnit::Repository::Distribution",
            &["CompUnit::Repository::Distribution"],
        ),
        S::LazyDistribution => (
            "CompUnit::Repository::Installation::LazyDistribution",
            &["CompUnit::Repository::Installation::LazyDistribution"],
        ),
        S::DistributionHash => ("Distribution::Hash", &["Distribution::Hash"]),
        S::DistributionPath => ("Distribution::Path", &["Distribution::Path"]),
        S::Supply => ("Supply", &["Supply"]),
        S::ProcAsync => ("Proc::Async", &["Proc::Async"]),
        S::Promise => ("Promise", &["Promise"]),
        S::PromiseVow => ("Promise::Vow", &["Promise::Vow"]),
        S::Channel => ("Channel", &["Channel"]),
        S::HyperSeq => ("HyperSeq", &["HyperSeq"]),
        S::RaceSeq => ("RaceSeq", &["RaceSeq"]),
        S::Distro => ("Distro", &["Distro"]),
        S::Kernel => ("Kernel", &["Kernel"]),
        S::Vm => ("VM", &["VM"]),
        S::RakuSystemic => ("Raku", &["Raku"]),
        S::Compiler => ("Compiler", &["Compiler"]),
        S::Stash => ("Stash", &["Stash"]),
        // An enum member by its payload kind: the roles Rakudo composes into
        // every enum type, the more specific first (ADR-11276 §9.125).
        S::EnumNumeric => ("NumericEnumeration", &["NumericEnumeration", "Enumeration"]),
        S::EnumStringy => ("StringyEnumeration", &["StringyEnumeration", "Enumeration"]),
        S::EnumGeneric => ("Enumeration", &["Enumeration"]),
        // The node classes whose builders are rows: each builder's handler
        // declines a node of another class, and no name is declared twice.
        S::RakuAstNode => (
            "RakuAST::Node",
            &[
                "RakuAST::Node",
                "RakuAST::StatementList",
                "RakuAST::CompUnit",
                "RakuAST::Statement::Expression",
                "RakuAST::ArgList",
            ],
        ),
        S::RakuAstOrigin => ("RakuAST::Origin", &["RakuAST::Origin"]),
        S::RakuAstOriginSource => ("RakuAST::Origin::Source", &["RakuAST::Origin::Source"]),
        _ => return None,
    })
}

/// The built-in classes whose instances have an opaque shape, beyond the one
/// each shape is named after: `Lock::Soft` is answered by `Lock`'s methods.
pub(super) const ALIASES: [(&str, DispatchShape); 2] = [
    ("Lock::Soft", DispatchShape::Lock),
    ("Perl", DispatchShape::RakuSystemic),
];
