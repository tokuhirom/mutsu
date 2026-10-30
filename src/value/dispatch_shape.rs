//! The receiver shapes the built-in method table keys on (issue #8888,
//! ADR-11276).
//!
//! A method call currently walks a gauntlet of receiver probes — the one in
//! `vm_native_dispatch::try_native_method_raw`, then `native_method_0arg`'s
//! prologue, then `dispatch_core`'s — before the family cascade that actually
//! answers it. Every probe re-decodes the same receiver and re-compares the
//! same method name, and for a *plain* value of one of these shapes none of
//! them can ever claim a call the table has a row for. [`DispatchShape`] is the
//! decoded receiver half of the `(shape, method) -> row` lookup in
//! `builtins::method_table`; each shape names the built-in type whose MRO the
//! lookup walks.
//!
//! Deliberately narrow: only the `Kind`s whose every value is an ordinary,
//! non-lazy, non-itemized value of one built-in type. Everything the gauntlet
//! exists for — a user `Instance`, a `Mixin`, a `Scalar`, a `Seq`, a
//! `LazyList`, a `Proxy`, a lazy `Match`, a shaped or lazy array, an itemized
//! hash — answers `None` and takes the ordinary path. (A `Match` of the built-in
//! class has a shape since slice 3D: the guard that refuses a lazy `Match` is that
//! no handler of its rows reads it through `view()`, which would materialize it.)
//!
//! # Adding a shape
//!
//! A shape is added by the slice whose first row needs it (ADR-11276 §10).
//! Three things make that safe, and none of them is the method:
//!
//! - **The decode refuses what the gauntlet refuses.** A tag probe
//!   (`NanBox::dispatch_shape`) for a value kind, or the class name of a
//!   built-in `Instance` ([`DispatchShape::from_instance_class`]): a user
//!   subclass has another class name and so has no shape.
//! - **A new shape is closed** ([`DispatchShape::inherits`]): only rows its own
//!   type owns reach it. A row owned by an ancestor (`Any.elems`, `Cool.uc`)
//!   was written for the shapes that existed when it was, so it reaches a new
//!   shape only once the slice that owns the shape has audited it and marked
//!   the shape open.
//! - **A type object is not an instance.** [`DispatchShape::from_type_symbol`]
//!   decodes `Package("Int")`; only a row flagged `TYPE_OBJECT_OK` answers it.

mod instances;
pub(super) mod iterator_kind;
mod opaque;

use crate::symbol::Symbol;
use instances::{instance_shapes, type_shapes};

/// The shape of a plain receiver, or of the type object of such a type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub(crate) enum DispatchShape {
    /// A plain `List` (itemized or not), never lazy.
    List,
    /// A plain `Array` (itemized or not), never shaped and never lazy.
    Array,
    /// A plain, non-itemized `Hash`.
    Hash,
    /// A `Str`.
    Str,
    /// A `Num` (an unboxed double, including `NaN` and the infinities).
    Num,
    /// An `Int`, inline, boxed or arbitrary-precision (not a `Bool`, an
    /// enum value or an `Int` subclass instance).
    Int,
    /// A `Rat`, with machine-word or arbitrary-precision components.
    Rat,
    /// A `FatRat`, with machine-word or arbitrary-precision components.
    FatRat,
    /// A `Complex`.
    Complex,
    /// A `Bool`. Its MRO is `Bool`, `Int`, `Cool`, `Any`, `Mu`; slice 3B
    /// audited those owners' rows for it and opened the shape.
    Bool,
    /// A `Range`, whatever its endpoints and exclusions (closed).
    Range,
    /// A `Pair`, of either flavour: a string-keyed one and a data one (closed).
    Pair,
    /// A `Capture` (closed).
    Capture,
    /// A `Version` (closed).
    Version,
    /// A `Uni` (closed).
    Uni,
    /// An immutable `Set` (closed).
    Set,
    /// A `SetHash` (closed).
    SetHash,
    /// An immutable `Bag` (closed).
    Bag,
    /// A `BagHash` (closed).
    BagHash,
    /// An immutable `Mix` (closed).
    Mix,
    /// A `MixHash` (closed).
    MixHash,
    /// A `Date`, the built-in class and not a subclass of it. Its MRO is
    /// `Date`, `Any`, `Mu`; slice 3D audited those owners' rows for it and
    /// opened the shape.
    Date,
    /// A `DateTime`, the built-in class and not a subclass of it. Its MRO is
    /// `DateTime`, `Any`, `Mu`; slice 3D audited those owners' rows for it and
    /// opened the shape.
    DateTime,
    /// An `Instant`, the built-in class and not a subclass of it. Its MRO is
    /// `Instant`, `Cool`, `Any`, `Mu`; slice 3D audited those owners' rows for
    /// it and opened the shape.
    Instant,
    /// A `Duration`, the built-in class and not a subclass of it. Its MRO is
    /// `Duration`, `Cool`, `Any`, `Mu`; slice 3D audited those owners' rows for
    /// it and opened the shape.
    Duration,
    /// A regex `Match` (lazy or eager), not a grammar cursor and not a
    /// subclass of it (closed). A method answers from the capture node when it
    /// can, so a lazy `Match` is not materialized by a call that does not need
    /// its structure.
    Match,
    /// An `IO::Path` or one of its SPEC variants (`IO::Path::Unix`, `Win32`,
    /// `Cygwin`, `QNX`), the built-in classes and not a user subclass of them
    /// (closed). The variants share the rows of `IO::Path`: a handler reads the
    /// instance's class name and `SPEC` attribute.
    IoPath,
    /// The type object `IO::Spec::Unix`, which is what `$*SPEC` is on POSIX. The
    /// `IO::Spec` shapes have no instances: nothing reads one, and the
    /// cascades answer a name like `join` for an instance by stringifying it.
    /// Closed to `Any` and `Mu`; it reaches the rows of `IO::Spec::Unix` and
    /// nothing else.
    IoSpecUnix,
    /// `IO::Spec::Win32`: its own rows and those of `IO::Spec::Unix`, which it
    /// is.
    IoSpecWin32,
    /// `IO::Spec::Cygwin`: its own rows and those of `IO::Spec::Unix`.
    IoSpecCygwin,
    /// `IO::Spec::QNX`: its own rows and those of `IO::Spec::Unix`.
    IoSpecQnx,
    /// An `IO::Handle`, the built-in class and not a user subclass of it
    /// (closed): a file, a standard stream or a socket-less pipe end whose live
    /// state is in the interpreter's handle table. A handler reads the instance
    /// only for its `handle` id and the attributes the handle was made with; a
    /// subclass (`IO::Socket::INET`, `IO::Pipe`, a user class) has another
    /// class name and reaches the rows through its owner (`invoke_owner`).
    IoHandle,
    /// A settled, plain `Seq`: reified, not lazy, not a `List` view (what a
    /// `.cache` returns) and not holding live element containers (closed). Only
    /// the entries that run after the Seq consumption step decode it
    /// ([`crate::builtins::method_table::Receiver::of_settled`]); the call-site
    /// lane and the by-name mutating entry run before that step and never see
    /// this shape, so a row cannot answer a `Seq` whose method would have consumed
    /// it.
    Seq,
    /// An immutable byte buffer: a `Blob` or `Blob[uintN]`/`Blob[intN]`, or one
    /// of the encoding buffers `utf8`, `utf16`, `utf32` (closed). Rakudo
    /// declares a copy of every `Blob` method on each of them, so the shape's
    /// rows are those of the owner `Blob`; a user subclass has another class
    /// name and no shape.
    Blob,
    /// A mutable byte buffer: a `Buf` or `Buf[uintN]`/`Buf[intN]` (closed). It
    /// reaches the rows of the owner `Buf`, the read-only ones registered once
    /// beside `Blob`'s and the mutators on `Buf` alone.
    Buf,
    /// A `Backtrace`, the built-in class and not a subclass of it (closed): a
    /// handler reads its `frames` and `text` attributes.
    Backtrace,
    /// A `Backtrace::Frame` (closed): a handler reads its `subname`, `file`,
    /// `line` and marker attributes.
    BacktraceFrame,
    /// The `Unicode` class (closed), a type object only: its `version` and
    /// `NFG` describe the Unicode data this runtime ships.
    Unicode,
    /// A `CompUnit::DependencySpecification` (closed): a handler reads its
    /// `short-name` and matcher attributes (ADR-11276 §9.115).
    CompUnitDepSpec,
    /// An `IO::Path::Parts` (closed): a handler reads its `volume`, `dirname`
    /// and `basename` attributes. `Any`'s list rows read it as one item, as
    /// Rakudo's do (ADR-11276 §9.114).
    IoPathParts,
    /// A `Format` (closed), Rakudo's 6.e `Str` subclass that is also a
    /// `Callable`: its string lives in the `Str` payload of a native subclass
    /// (`__mutsu_str_value`), which every method but its own rows reads
    /// (ADR-11276 §9.153).
    Format,
    /// The `Formatter` class (closed), a type object only: `CODE` and `AST`
    /// compile a format string.
    Formatter,
    /// A loop `Label` (closed): a handler reads its `name`, `file` and `line`
    /// attributes, and `next`/`last`/`redo` raise the labelled loop-control
    /// signal.
    Label,
    /// The type object `Rakudo::Internals` (closed), a type object only: the few
    /// methods ecosystem code calls on it (`IS-WIN`, `INCLUDE`, `INITTIME`).
    RakudoInternals,
    /// The type object `Rakudo::Sorting` (closed), a type object only:
    /// `MERGESORT-str`.
    RakudoSorting,
    /// A `Collation` (`$*COLLATION`), the built-in class and not a subclass of
    /// it (closed): a handler reads and writes its four level attributes.
    Collation,
    /// An `Attribute` metaobject (closed): a handler reads the declaration's
    /// facts from its attributes.
    Attribute,
    /// A `Variable`, the reflective object a variable trait handler receives
    /// (closed).
    Variable,
    // The opaque instance shapes (`opaque.rs`): an instance of a built-in
    // class whose state lives in the interpreter or in its attributes, and
    // whose rows are its class's own. Each reaches the owners
    // [`DispatchShape::opaque_owners`] names and nothing else.
    /// `Lock` or `Lock::Soft`, which Rakudo's `Lock` methods answer (opaque).
    Lock,
    /// `Lock::Async` (opaque).
    LockAsync,
    /// `Lock::ConditionVariable` (opaque).
    LockConditionVariable,
    /// `Cancellation` (opaque).
    Cancellation,
    /// `Semaphore` (opaque).
    Semaphore,
    /// `Thread` (opaque).
    Thread,
    /// `Tap` (opaque).
    Tap,
    /// `IO::Socket::Async::ListenSocket`, a `Tap` (opaque).
    ListenSocket,
    /// `Proc` (opaque).
    Proc,
    /// `IO::Socket::INET` (opaque).
    SocketInet,
    /// `IO::Socket::Async` (opaque).
    SocketAsync,
    /// `IO::CatHandle` (opaque).
    CatHandle,
    /// `ThreadPoolScheduler` (opaque).
    ThreadPoolScheduler,
    /// `CurrentThreadScheduler` (opaque).
    CurrentThreadScheduler,
    /// `Encoding::Builtin` (opaque).
    EncodingBuiltin,
    /// `Encoding::Encoder::Builtin` (opaque).
    EncodingEncoder,
    /// `Encoding::Decoder::Builtin` (opaque).
    EncodingDecoder,
    /// `WalkList` (opaque).
    WalkList,
    /// `Routine::WrapHandle` (opaque).
    WrapHandle,
    /// `HyperConfiguration` (opaque).
    HyperConfiguration,
    /// `Pod::Block::Declarator` (opaque).
    PodDeclarator,
    /// `CompUnit` (opaque).
    CompUnit,
    /// `CompUnit::Handle` (opaque).
    CompUnitHandle,
    /// `CompUnit::Repository::Installation` (opaque).
    RepoInstallation,
    /// `CompUnit::Repository::FileSystem` (opaque).
    RepoFileSystem,
    /// `CompUnit::Repository::Distribution` (opaque).
    RepoDistribution,
    /// `CompUnit::Repository::Installation::LazyDistribution` (opaque).
    LazyDistribution,
    /// `Distribution::Hash` (opaque).
    DistributionHash,
    /// `Distribution::Path` (opaque).
    DistributionPath,
    /// `Supply` (opaque).
    Supply,
    /// `Proc::Async` (opaque).
    ProcAsync,
    /// `Promise` (opaque): a value kind, not an instance.
    Promise,
    /// `Promise::Vow` (opaque).
    PromiseVow,
    /// `Channel` (opaque): a value kind.
    Channel,
    /// `HyperSeq` (opaque): a value kind.
    HyperSeq,
    /// `RaceSeq` (opaque): a value kind.
    RaceSeq,
    /// `Distro` (opaque).
    Distro,
    /// `Kernel` (opaque).
    Kernel,
    /// `VM` (opaque).
    Vm,
    /// `Raku` (and `Perl`) (opaque).
    RakuSystemic,
    /// `Compiler` (opaque).
    Compiler,
    /// A built-in iterator that is not a `PredictiveIterator` (opaque; the
    /// shape is decoded from the instance's attributes, `iterator_kind.rs`).
    Iterator,
    /// A built-in iterator that knows its count (opaque).
    PredictiveIterator,
    /// `Stash` (opaque): its associative indexing stays with the hash paths,
    /// which read [`Value::dispatch_shape`](crate::value::Value::dispatch_shape).
    Stash,
    /// `Signature` (opaque): its renderings and parameter list are
    /// attributes the compiler set.
    Signature,
    /// A member of an `Int`-valued enum (opaque): a value kind, answering the
    /// rows of the roles Rakudo composes into the enum type.
    EnumNumeric,
    /// A member of a `Str`-valued enum (opaque).
    EnumStringy,
    /// A member of an enum with any other payload (opaque).
    EnumGeneric,
    /// A RakuAST node (opaque): a value kind, answering the rows of the node
    /// classes whose builders are rows.
    RakuAstNode,
    /// `RakuAST::Origin` (opaque).
    RakuAstOrigin,
    /// `RakuAST::Origin::Source` (opaque).
    RakuAstOriginSource,
    /// `RakuAST::Origin::Match` (opaque).
    RakuAstOriginMatch,
    /// `RakuAST::Declaration::External::Setting` (opaque).
    RakuAstDeclarationExternalSetting,
    /// `RakuAST::Declaration::External::Constant` (opaque).
    RakuAstDeclarationExternalConstant,
    /// `RakuAST::Declaration::Import` (opaque).
    RakuAstDeclarationImport,
    /// `RakuAST::Declaration::LexicalPackage` (opaque).
    RakuAstDeclarationLexicalPackage,
}

impl DispatchShape {
    /// Every shape, in declaration order. The call-site memo packs a shape
    /// into seven bits of a byte (the eighth marks a type object) and the
    /// table keeps a bit of a `u128` per shape, so this stays under 128.
    pub(crate) const ALL: [DispatchShape; 104] = [
        DispatchShape::List,
        DispatchShape::Array,
        DispatchShape::Hash,
        DispatchShape::Str,
        DispatchShape::Num,
        DispatchShape::Int,
        DispatchShape::Rat,
        DispatchShape::FatRat,
        DispatchShape::Complex,
        DispatchShape::Bool,
        DispatchShape::Range,
        DispatchShape::Pair,
        DispatchShape::Capture,
        DispatchShape::Version,
        DispatchShape::Uni,
        DispatchShape::Set,
        DispatchShape::SetHash,
        DispatchShape::Bag,
        DispatchShape::BagHash,
        DispatchShape::Mix,
        DispatchShape::MixHash,
        DispatchShape::Date,
        DispatchShape::DateTime,
        DispatchShape::Instant,
        DispatchShape::Duration,
        DispatchShape::Match,
        DispatchShape::IoPath,
        DispatchShape::IoSpecUnix,
        DispatchShape::IoSpecWin32,
        DispatchShape::IoSpecCygwin,
        DispatchShape::IoSpecQnx,
        DispatchShape::IoHandle,
        DispatchShape::Seq,
        DispatchShape::Blob,
        DispatchShape::Buf,
        DispatchShape::Backtrace,
        DispatchShape::BacktraceFrame,
        DispatchShape::Unicode,
        DispatchShape::CompUnitDepSpec,
        DispatchShape::IoPathParts,
        DispatchShape::Format,
        DispatchShape::Formatter,
        DispatchShape::Label,
        DispatchShape::RakudoInternals,
        DispatchShape::RakudoSorting,
        DispatchShape::Collation,
        DispatchShape::Attribute,
        DispatchShape::Variable,
        DispatchShape::Lock,
        DispatchShape::LockAsync,
        DispatchShape::LockConditionVariable,
        DispatchShape::Cancellation,
        DispatchShape::Semaphore,
        DispatchShape::Thread,
        DispatchShape::Tap,
        DispatchShape::ListenSocket,
        DispatchShape::Proc,
        DispatchShape::SocketInet,
        DispatchShape::SocketAsync,
        DispatchShape::CatHandle,
        DispatchShape::ThreadPoolScheduler,
        DispatchShape::CurrentThreadScheduler,
        DispatchShape::EncodingBuiltin,
        DispatchShape::EncodingEncoder,
        DispatchShape::EncodingDecoder,
        DispatchShape::WalkList,
        DispatchShape::WrapHandle,
        DispatchShape::HyperConfiguration,
        DispatchShape::PodDeclarator,
        DispatchShape::CompUnit,
        DispatchShape::CompUnitHandle,
        DispatchShape::RepoInstallation,
        DispatchShape::RepoFileSystem,
        DispatchShape::RepoDistribution,
        DispatchShape::LazyDistribution,
        DispatchShape::DistributionHash,
        DispatchShape::DistributionPath,
        DispatchShape::Supply,
        DispatchShape::ProcAsync,
        DispatchShape::Promise,
        DispatchShape::PromiseVow,
        DispatchShape::Channel,
        DispatchShape::HyperSeq,
        DispatchShape::RaceSeq,
        DispatchShape::Distro,
        DispatchShape::Kernel,
        DispatchShape::Vm,
        DispatchShape::RakuSystemic,
        DispatchShape::Compiler,
        DispatchShape::EnumNumeric,
        DispatchShape::EnumStringy,
        DispatchShape::EnumGeneric,
        DispatchShape::RakuAstNode,
        DispatchShape::RakuAstOrigin,
        DispatchShape::RakuAstOriginSource,
        DispatchShape::RakuAstOriginMatch,
        DispatchShape::RakuAstDeclarationExternalSetting,
        DispatchShape::RakuAstDeclarationExternalConstant,
        DispatchShape::RakuAstDeclarationImport,
        DispatchShape::RakuAstDeclarationLexicalPackage,
        DispatchShape::Iterator,
        DispatchShape::PredictiveIterator,
        DispatchShape::Stash,
        DispatchShape::Signature,
    ];

    /// The built-in type whose MRO a receiver of this shape is dispatched
    /// along.
    // Cost: O(1).
    pub(crate) const fn type_name(self) -> &'static str {
        match self {
            DispatchShape::List => "List",
            DispatchShape::Array => "Array",
            DispatchShape::Hash => "Hash",
            DispatchShape::Str => "Str",
            DispatchShape::Num => "Num",
            DispatchShape::Int => "Int",
            DispatchShape::Rat => "Rat",
            DispatchShape::FatRat => "FatRat",
            DispatchShape::Complex => "Complex",
            DispatchShape::Bool => "Bool",
            DispatchShape::Range => "Range",
            DispatchShape::Pair => "Pair",
            DispatchShape::Capture => "Capture",
            DispatchShape::Version => "Version",
            DispatchShape::Uni => "Uni",
            DispatchShape::Set => "Set",
            DispatchShape::SetHash => "SetHash",
            DispatchShape::Bag => "Bag",
            DispatchShape::BagHash => "BagHash",
            DispatchShape::Mix => "Mix",
            DispatchShape::MixHash => "MixHash",
            DispatchShape::Date => "Date",
            DispatchShape::DateTime => "DateTime",
            DispatchShape::Instant => "Instant",
            DispatchShape::Duration => "Duration",
            DispatchShape::Match => "Match",
            DispatchShape::IoPath => "IO::Path",
            DispatchShape::IoSpecUnix => "IO::Spec::Unix",
            DispatchShape::IoSpecWin32 => "IO::Spec::Win32",
            DispatchShape::IoSpecCygwin => "IO::Spec::Cygwin",
            DispatchShape::IoSpecQnx => "IO::Spec::QNX",
            DispatchShape::IoHandle => "IO::Handle",
            DispatchShape::Seq => "Seq",
            DispatchShape::Blob => "Blob",
            DispatchShape::Buf => "Buf",
            DispatchShape::Backtrace => "Backtrace",
            DispatchShape::BacktraceFrame => "Backtrace::Frame",
            DispatchShape::Unicode => "Unicode",
            DispatchShape::CompUnitDepSpec => "CompUnit::DependencySpecification",
            DispatchShape::IoPathParts => "IO::Path::Parts",
            DispatchShape::Format => "Format",
            DispatchShape::Formatter => "Formatter",
            DispatchShape::Label => "Label",
            DispatchShape::RakudoInternals => "Rakudo::Internals",
            DispatchShape::RakudoSorting => "Rakudo::Sorting",
            DispatchShape::Collation => "Collation",
            DispatchShape::Attribute => "Attribute",
            DispatchShape::Variable => "Variable",
            shape => match opaque::opaque(shape) {
                Some((name, _)) => name,
                None => panic!("every shape has a type name"),
            },
        }
    }

    /// The owners an opaque instance shape's rows are found under, nearest
    /// first (`opaque.rs`); `None` for every other shape.
    // Cost: O(1).
    pub(crate) const fn opaque_owners(self) -> Option<&'static [&'static str]> {
        match opaque::opaque(self) {
            Some((_, owners)) => Some(owners),
            None => None,
        }
    }

    /// Whether this is an opaque instance shape (`opaque.rs`): it reaches its
    /// own owners' rows and no ancestor's.
    // Cost: O(1).
    pub(crate) const fn is_opaque(self) -> bool {
        self.opaque_owners().is_some()
    }

    /// Whether a value can be an instance of this shape. The `IO::Spec` shapes
    /// and `Unicode` are type objects only.
    // Cost: O(1).
    pub(crate) const fn has_instances(self) -> bool {
        !matches!(
            self,
            DispatchShape::IoSpecUnix
                | DispatchShape::IoSpecWin32
                | DispatchShape::IoSpecCygwin
                | DispatchShape::IoSpecQnx
                | DispatchShape::Unicode
                | DispatchShape::Formatter
                | DispatchShape::RakudoInternals
                | DispatchShape::RakudoSorting
        )
    }

    /// Whether the rows `owner` declares reach this shape. An open shape
    /// ([`Self::inherits`]) reaches every owner in its type's MRO; a closed
    /// one reaches its own type's rows and those of the ancestors it names
    /// here, which its slice has audited (the `IO::Spec` family reaches
    /// `IO::Spec::Unix`, whose methods its classes inherit, and never `Any`
    /// or `Mu`). A role's rows reach every shape whose type composes it
    /// ([`Self::reaches_role`]).
    // Cost: O(r), r = audited roles.
    pub(crate) const fn reaches(self, owner: &str, method: &str) -> bool {
        self.reaches_owner(owner) || self.reaches_audited(owner, method) || self.reaches_role(owner)
    }

    /// Whether the rows of the role `role`, which this shape's type composes,
    /// reach it (ADR-11276 §9.86). A role is opened as a whole, for every shape
    /// whose type composes it, open or closed, once its rows were audited for
    /// all of them; [`ROLES_WITH_ROWS`] lists the roles that were.
    // Cost: O(r), r = audited roles.
    pub(crate) const fn reaches_role(self, role: &str) -> bool {
        !self.is_opaque() && const_list_contains(&ROLES_WITH_ROWS, role)
    }

    /// [`Self::reaches`] for the rows of `owner` as a whole, before the
    /// per-method audit of `Cool` for the closed collection shapes.
    // Cost: O(1).
    pub(crate) const fn reaches_owner(self, owner: &str) -> bool {
        if let Some(owners) = self.opaque_owners() {
            return const_list_contains(owners, owner);
        }
        self.inherits()
            || const_str_eq(owner, self.type_name())
            || (matches!(
                self,
                DispatchShape::IoSpecWin32 | DispatchShape::IoSpecCygwin | DispatchShape::IoSpecQnx
            ) && const_str_eq(owner, "IO::Spec::Unix"))
    }

    /// Whether some row of `owner` may reach this shape through the per-method
    /// audit ([`Self::reaches`] decides which).
    // Cost: O(1).
    pub(crate) const fn may_reach_audited(self, owner: &str) -> bool {
        (matches!(
            self,
            DispatchShape::Range
                | DispatchShape::Seq
                | DispatchShape::IoPath
                | DispatchShape::Match
        ) && const_str_eq(owner, "Cool"))
            || (matches!(self, DispatchShape::Match) && const_str_eq(owner, "Capture"))
            || (self.reaches_audited_any() && const_str_eq(owner, "Any"))
            || (!self.inherits()
                && !self.answers_on_payload()
                && !self.is_opaque()
                && const_str_eq(owner, "Mu"))
    }

    /// Whether this shape's inherited methods are answered on the native
    /// payload its values carry (`Format`'s `Str`, `builtins::native_base`),
    /// so no ancestor row reaches it (ADR-11276 §9.153). Its type object has
    /// no payload and so no shape (`NanBox::type_object_shape`).
    // Cost: O(1).
    pub(crate) const fn answers_on_payload(self) -> bool {
        matches!(self, DispatchShape::Format)
    }

    /// Whether this closed shape reaches some of `Any`'s list rows
    /// ([`ANY_LIST_FOR_CLOSED_SHAPES`]; [`Self::reaches_audited_any_method`]
    /// says which).
    // Cost: O(1).
    const fn reaches_audited_any(self) -> bool {
        !self.inherits() && self.has_instances() && !self.answers_on_payload() && !self.is_opaque()
    }

    /// Whether `Any`'s list row `method` reaches this closed shape. None
    /// reaches `sort`, whose handler reads only a list (a closed shape sorts
    /// through the interpreter), and only a `Pair`, `Capture` or `Version` reaches `reverse`, a `Pair` `min`
    /// and `max`, whose handlers read a scalar, a hash, a list and a pair. A `Capture` flattens its
    /// positionals (`Value::match_list_receiver`), and the
    /// quant hashes read as their list of pairs for `unique` first
    /// (`hashlike_receiver_as_pairs_list`), then reach the row as a `List`;
    /// the rest reach the row with themselves as invocant.
    // Cost: O(m), m = audited names.
    const fn reaches_audited_any_method(self, method: &str) -> bool {
        if !const_list_contains(&ANY_LIST_FOR_CLOSED_SHAPES, method) {
            return false;
        }
        match self {
            // A `Match` joins its positional captures, counts them for `end`
            // and is its own `serial`; the others' handlers do not read a
            // `Match`'s captures (ADR-11276 §9.96).
            _ if const_str_eq(method, "ACCEPTS") => matches!(self, DispatchShape::Capture),
            DispatchShape::Match => {
                const_str_eq(method, "join")
                    || const_str_eq(method, "serial")
                    || const_str_eq(method, "end")
                    // `Any.tree` of a non-`Iterable` is the value itself.
                    || const_str_eq(method, "tree")
            }
            _ if const_str_eq(method, "sort") => false,
            // `min`/`max` read a `Pair` as its one element; the other closed
            // shapes take the interpreter's extremum walk.
            _ if const_str_eq(method, "min") || const_str_eq(method, "max") => {
                matches!(self, DispatchShape::Pair)
            }
            DispatchShape::Pair => true,
            // A `Backtrace` answers them on its frames
            // (`Value::match_list_receiver`, ADR-11276 §9.111).
            DispatchShape::Backtrace => false,
            // `Any.reverse` is `self.list.reverse`: a `Capture` reads its
            // positionals, a `Version` is the one-element list holding itself.
            _ if const_str_eq(method, "reverse") => {
                matches!(self, DispatchShape::Capture | DispatchShape::Version)
            }
            DispatchShape::Capture => !const_str_eq(method, "flat"),
            DispatchShape::Set
            | DispatchShape::SetHash
            | DispatchShape::Bag
            | DispatchShape::BagHash
            | DispatchShape::Mix
            | DispatchShape::MixHash => {
                !const_str_eq(method, "reverse") && !const_str_eq(method, "unique")
            }
            _ => true,
        }
    }

    /// Whether `Cool`'s `method` row is one a closed `Range`, `Seq`, `IO::Path`
    /// or `Match` shape reaches. A `Range` or `Seq` reaches the numeric methods
    /// whose Rakudo body is `self.Numeric.METHOD` and whose handlers read the
    /// receiver through `numify` (a `Range` or `Seq` numifies to its element
    /// count), and `Version`, which reads its string form (`(1..3).Version` is
    /// `v1.2.3`). All three reach the text methods whose Rakudo body is
    /// `self.Str.METHOD` and whose handlers read the receiver's string form
    /// (`"x ".IO.trim` is `"x"`, `(1..3).flip` is `"3 2 1"`); `IO::Path`'s own
    /// `lines`, `words` and `comb` come first in its MRO. A `Match` reaches the
    /// text methods and, besides them, the search, split and substitution rows
    /// ([`COOL_TEXT_FOR_MATCH`]), but no numeric one: `Match` declares its own
    /// `Int`/`Num`/`Numeric`. Audited per method (ADR-11276 §9.16, §9.79,
    /// §9.89), not per owner, so the rows `Cool` declares beyond these stay
    /// unreachable.
    // Cost: O(m), m = audited names.
    const fn reaches_audited(self, owner: &str, method: &str) -> bool {
        if !self.may_reach_audited(owner) {
            return false;
        }
        if const_str_eq(owner, "Any") {
            return self.reaches_audited_any_method(method);
        }
        if const_str_eq(owner, "Mu") {
            // An `IO::Path::Parts` renders as an object (`IO::Path::Parts<id>`),
            // which only the interpreter's default representation answers.
            if matches!(
                self,
                DispatchShape::IoPathParts
                    | DispatchShape::Collation
                    | DispatchShape::Variable
                    | DispatchShape::Attribute
            ) && (const_str_eq(method, "Str") || const_str_eq(method, "Stringy"))
            {
                return false;
            }
            // An instance of a built-in class with no rendering of its own
            // (`Backtrace::Frame`) renders as its attribute dump, which only the
            // interpreter's default representation answers (§9.162).
            if matches!(
                self,
                DispatchShape::BacktraceFrame
                    | DispatchShape::CompUnitDepSpec
                    | DispatchShape::Backtrace
                    | DispatchShape::IoPathParts
                    | DispatchShape::Variable
                    | DispatchShape::Collation
                    | DispatchShape::Attribute
            ) && (const_str_eq(method, "gist") || const_str_eq(method, "raku"))
            {
                return false;
            }
            return const_list_contains(&MU_FOR_CLOSED_SHAPES, method)
                || self.reaches_mu_numeric(method);
        }
        if const_str_eq(owner, "Capture") {
            return const_list_contains(&CAPTURE_FOR_MATCH, method);
        }
        if matches!(self, DispatchShape::Match) {
            return const_list_contains(&COOL_TEXT_FOR_CLOSED_SHAPES, method)
                || const_list_contains(&COOL_TEXT_FOR_MATCH, method)
                // `Match` declares `Int`/`Num`/`Numeric`, not `UInt` or the
                // numeric methods: `Cool`'s read the matched text through
                // `numify` (§9.92, §9.96). `Match.Num` is its own row.
                || const_str_eq(method, "UInt")
                || const_list_contains(&COOL_NUMERIC_FOR_COLLECTIONS, method)
                || const_list_contains(&COOL_COERCE_FOR_MATCH, method);
        }
        const_list_contains(&COOL_TEXT_FOR_CLOSED_SHAPES, method)
            || (!matches!(self, DispatchShape::IoPath)
                && const_list_contains(&COOL_NUMERIC_FOR_COLLECTIONS, method))
            // `IO::Path.chars` is `Cool.chars` of the path (§9.112).
            || (matches!(self, DispatchShape::IoPath) && const_str_eq(method, "chars"))
    }

    /// Whether `Mu`'s `Int`/`Numeric` row (`method_table::ctors_mop::mu_numeric`)
    /// reaches this closed shape (ADR-11276 §9.92): a `Range` numifies to its
    /// element count (Rakudo's `Range` declares `Numeric`, not `Int`) and a
    /// `Blob`/`Buf` to its length. The other closed shapes declare their own
    /// or have none in Rakudo either.
    // Cost: O(1).
    const fn reaches_mu_numeric(self, method: &str) -> bool {
        match self {
            DispatchShape::Range => const_str_eq(method, "Int"),
            DispatchShape::Blob | DispatchShape::Buf => {
                const_str_eq(method, "Int") || const_str_eq(method, "Numeric")
            }
            _ => false,
        }
    }

    /// Whether rows owned by an ancestor of this shape's type reach it. The
    /// nine shapes the table started with inherit, and so do `Bool` (opened by
    /// slice 3B) and `Date`, `DateTime`, `Instant` and `Duration` (opened by
    /// slice 3D); a shape added later is closed until the slice that owns it
    /// has audited every ancestor row for it (see the module docs).
    // Cost: O(1).
    pub(crate) const fn inherits(self) -> bool {
        matches!(
            self,
            DispatchShape::List
                | DispatchShape::Array
                | DispatchShape::Hash
                | DispatchShape::Str
                | DispatchShape::Num
                | DispatchShape::Int
                | DispatchShape::Rat
                | DispatchShape::FatRat
                | DispatchShape::Complex
                | DispatchShape::Bool
                | DispatchShape::Date
                | DispatchShape::DateTime
                | DispatchShape::Instant
                | DispatchShape::Duration
        )
    }

    /// The shape of the type object named `name` (`Package("Int")`).
    // Cost: O(1), one hash lookup.
    #[inline]
    pub(crate) fn from_type_symbol(name: Symbol) -> Option<DispatchShape> {
        type_shapes().get(&name).copied()
    }

    /// The shape of an `Instance` of the built-in class `class`, for the
    /// shapes whose values are instances.
    // Cost: O(1), one hash lookup (plus a fixed-length prefix compare on a
    // miss).
    #[inline]
    pub(crate) fn from_instance_class(class: Symbol) -> Option<DispatchShape> {
        instance_shapes().get(&class).copied().or_else(|| {
            // An `Attribute` reblessed by `$attr does R` keeps answering the
            // meta-object's own methods; the mixin type's role methods win
            // over them through the user-override check every row entry makes.
            crate::value::attribute_class::is_attribute_class(class)
                .then_some(DispatchShape::Attribute)
        })
    }
}

/// The roles whose rows reach every shape whose type composes them
/// ([`DispatchShape::reaches_role`]). Their only rows are the element and key
/// type methods `of`/`keyof` (§9.86), which read nothing but the receiver's
/// type and its container descriptor; a role gaining another row is audited
/// for every composing shape first.
pub(crate) const ROLES_WITH_ROWS: [&str; 8] = [
    "Positional",
    "Associative",
    "Callable",
    "Baggy",
    "Setty",
    "Mixy",
    "QuantHash",
    // `Numeric.ACCEPTS` (§9.108), the smartmatch every numeric shape agrees
    // with.
    "Numeric",
];

/// The `Cool` numeric methods a `Range` or `Seq` reaches
/// ([`DispatchShape::reaches_audited`]).
const COOL_NUMERIC_FOR_COLLECTIONS: [&str; 53] = [
    "sin", "cos", "tan", "sec", "cosec", "cotan", "sinh", "cosh", "tanh", "sech", "cosech",
    "cotanh", "asin", "acos", "atan", "asec", "acosec", "acotan", "asinh", "acosh", "atanh",
    "asech", "acosech", "acotanh", "exp", "log", "log2", "log10", "sqrt", "cis", "atan2", "roots",
    "unpolar", "abs", "sign", "floor", "ceiling", "truncate", "round", "is-prime", "conj", "chr",
    "rand", "int", "uint", "byte", "Complex", "UInt", "Version", "Real", "Num", "Rat", "FatRat",
];

/// Whether `Cool` declares `name` as `self.Numeric.METHOD` (or a numeric
/// coercion), so a receiver with both a string and a number (an allomorph,
/// `method_table::allomorph`) answers it on its number.
// Cost: O(m), m = names in [`COOL_NUMERIC_FOR_COLLECTIONS`].
pub(crate) fn cool_numeric_method(name: &str) -> bool {
    COOL_NUMERIC_FOR_COLLECTIONS.contains(&name)
}

/// The `Cool` text methods a `Range`, `Seq`, `IO::Path` or `Match` reaches
/// ([`DispatchShape::reaches_audited`]), and every receiver with no shape
/// that is a `Cool` (`method_table::answer_shapeless`). Each reads the
/// receiver's string form; the last four take an argument (§9.90).
pub(crate) const COOL_TEXT_FOR_CLOSED_SHAPES: [&str; 34] = [
    "words",
    "codes",
    "lines",
    "trim",
    "trim-leading",
    "trim-trailing",
    "flip",
    "chomp",
    "chop",
    "comb",
    "fmt",
    "uc",
    "lc",
    "fc",
    "tc",
    "tclc",
    "wordcase",
    "encode",
    "NFC",
    "NFD",
    "NFKC",
    "NFKD",
    "ord",
    "ords",
    "uniname",
    "uninames",
    "uniprop",
    "uniprops",
    "unival",
    "univals",
    "unimatch",
    "samecase",
    "samemark",
    "indent",
];

/// The `Cool` text methods with arguments a `Match` reaches besides
/// [`COOL_TEXT_FOR_CLOSED_SHAPES`] ([`DispatchShape::reaches_audited`];
/// ADR-11276 §9.89). Rakudo's `Match` is `Capture is Cool`, and neither
/// `Match` nor `Capture` declares these, so each is `Cool`'s candidate, whose
/// body is `self.Str.METHOD`: the handlers read the receiver's string form,
/// which for a `Match` is the matched text.
const COOL_TEXT_FOR_MATCH: [&str; 12] = [
    "contains",
    "starts-with",
    "ends-with",
    "index",
    "rindex",
    "substr",
    "split",
    "subst",
    "trans",
    "indices",
    "sprintf",
    "IO",
];

/// The `Cool` coercions and parsers a `Match` reaches besides the numeric
/// methods ([`DispatchShape::reaches_audited`]; ADR-11276 §9.96): neither
/// `Match` nor `Capture` declares them, so each is `Cool`'s candidate, which
/// reads the matched text (`self.Str.METHOD` or `self.Numeric.METHOD`).
const COOL_COERCE_FOR_MATCH: [&str; 6] =
    ["Rat", "FatRat", "Failure", "parse-names", "uniparse", "AST"];

/// The `Any` list methods every closed shape with instances reaches
/// ([`DispatchShape::reaches_audited_any`]): Rakudo's `Any` declares them as
/// `self.list.METHOD`, and their handlers read any receiver as its list (a
/// `Pair` or a `Set` as one element or its pairs, a `Blob` as its bytes).
/// A shape whose own type declares one of them (`Seq.reverse`, `Range.flat`)
/// answers its own row first. `combinations` and `batch` read any receiver
/// through `value_to_list_for_receiver` (§9.100). `join` (`self.list.join`) reads a `Pair` as its
/// `.Str`, a `Capture` as its positionals and a quant hash as its pairs
/// (`method_table::collections::join`). `serial` is the receiver itself.
pub(crate) const ANY_LIST_FOR_CLOSED_SHAPES: [&str; 42] = [
    // `Any`'s list coercions (`method_table::any_coerce`, §9.110).
    "List",
    "list",
    "Array",
    "Slip",
    "Supply",
    // `Any`'s views and the other `self.list.METHOD` methods whose handlers
    // carry the deleted cascade arms' bodies (`method_table::any_views`,
    // §9.109): a closed shape that declares its own (`Pair.keys`,
    // `Capture.hash`, `Bag.invert`) answers from it first.
    "keys",
    "values",
    "kv",
    "pairs",
    "antipairs",
    "invert",
    "pairup",
    "permutations",
    "cache",
    "hash",
    "sum",
    "flat",
    "sort",
    "unique",
    "repeated",
    "reverse",
    "end",
    "elems",
    "join",
    "pick",
    "roll",
    "minmax",
    "head",
    "tail",
    "min",
    "max",
    "serial",
    "combinations",
    "batch",
    // `Any`'s subscript protocol (§9.106): a value that is not `Positional`
    // is the one-element list holding itself, and has no keys.
    "AT-POS",
    "EXISTS-POS",
    "AT-KEY",
    "EXISTS-KEY",
    // `Any.ACCEPTS` (`self === topic`, §9.108) reaches a `Capture` alone: the
    // other closed shapes declare their own or are not audited for it.
    "ACCEPTS",
    // `Any.tree` (ADR-11276 §9.112): a list-like shape trees its elements, the
    // others answer themselves.
    "tree",
    // `Any.chrs` (§9.112): `self.list.chrs`.
    "chrs",
    // `Any.lazy-if` ignores its flag: the receiver itself.
    "lazy-if",
];

/// The `Capture` rows a `Match` reaches ([`DispatchShape::reaches_audited`];
/// ADR-11276 §9.95): Rakudo's `Match` is a `Capture` and declares none of
/// them, and their handlers (`method_table::collections::capture`) read a
/// `Match`'s captures as its parts, `list` as a live view of its positional
/// captures and `Capture` as the `Match` itself (§9.96).
pub(crate) const CAPTURE_FOR_MATCH: [&str; 11] = [
    "keys",
    "values",
    "kv",
    "pairs",
    "antipairs",
    "hash",
    "elems",
    "list",
    "Capture",
    "AT-POS",
    "EXISTS-POS",
];

/// The `Mu` methods every closed shape reaches ([`DispatchShape::reaches_audited`])
/// and every receiver with no shape (`method_table::answer_shapeless`): their
/// handlers read only the receiver's truth or its lazy flag, wrap it in an item
/// container, sink it, return it (`return`), walk its class (`WALK`), guard its
/// rendering (`rakuseen`), or render it (`Str`, `Stringy`, which a shape with its own
/// `Str` row answers first) or ask its type (`isa`), which every value supports. A shape whose own type declares one (`Match.not`, `Range.is-lazy`)
/// answers its own row first.
pub(crate) const MU_FOR_CLOSED_SHAPES: [&str; 22] = [
    "so", "not", "is-lazy", "item", "sink", "self", "defined", "DEFINITE", "Bool", "WHICH",
    "WHERE", "clone", "Str", "Stringy", "isa", "Capture", "gist", "raku", "emit", "rakuseen",
    "WALK", "return",
];

/// Whether `list` contains `name`, for a `const fn`.
// Cost: O(n), n = entries of `list`.
const fn const_list_contains(list: &[&str], name: &str) -> bool {
    let mut i = 0;
    while i < list.len() {
        if const_str_eq(name, list[i]) {
            return true;
        }
        i += 1;
    }
    false
}

/// `a == b` for a `const fn`, where `==` on `str` is not yet available.
// Cost: O(n), n = the shorter length.
pub(crate) const fn const_str_eq(a: &str, b: &str) -> bool {
    let (a, b) = (a.as_bytes(), b.as_bytes());
    if a.len() != b.len() {
        return false;
    }
    let mut i = 0;
    while i < a.len() {
        if a[i] != b[i] {
            return false;
        }
        i += 1;
    }
    true
}
