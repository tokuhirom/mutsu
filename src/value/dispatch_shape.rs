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
//! - **A type object is not an instance.** [`DispatchShape::from_type_name`]
//!   decodes `Package("Int")`; only a row flagged `TYPE_OBJECT_OK` answers it.

use crate::symbol::Symbol;
use std::sync::OnceLock;

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
}

impl DispatchShape {
    /// Every shape, in declaration order. The call-site memo packs a shape
    /// into one byte and the table keeps a bit per shape, so this stays under
    /// 64.
    pub(crate) const ALL: [DispatchShape; 31] = [
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
        }
    }

    /// Whether a value can be an instance of this shape. The `IO::Spec` shapes
    /// are type objects only.
    // Cost: O(1).
    pub(crate) const fn has_instances(self) -> bool {
        !matches!(
            self,
            DispatchShape::IoSpecUnix
                | DispatchShape::IoSpecWin32
                | DispatchShape::IoSpecCygwin
                | DispatchShape::IoSpecQnx
        )
    }

    /// Whether the rows `owner` declares reach this shape. An open shape
    /// ([`Self::inherits`]) reaches every owner in its type's MRO; a closed
    /// one reaches its own type's rows and those of the ancestors it names
    /// here, which its slice has audited (the `IO::Spec` family reaches
    /// `IO::Spec::Unix`, whose methods its classes inherit, and never `Any`
    /// or `Mu`).
    // Cost: O(1).
    pub(crate) fn reaches(self, owner: &str) -> bool {
        self.inherits()
            || owner == self.type_name()
            || (matches!(
                self,
                DispatchShape::IoSpecWin32 | DispatchShape::IoSpecCygwin | DispatchShape::IoSpecQnx
            ) && owner == "IO::Spec::Unix")
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
    // Cost: O(s), s = shapes.
    pub(crate) fn from_type_name(name: &str) -> Option<DispatchShape> {
        DispatchShape::ALL
            .into_iter()
            .find(|shape| shape.type_name() == name)
    }

    /// The shape of an `Instance` of the built-in class `class`, for the
    /// shapes whose values are instances.
    // Cost: O(1), one symbol compare per instance-backed shape.
    pub(crate) fn from_instance_class(class: Symbol) -> Option<DispatchShape> {
        instance_shapes()
            .iter()
            .find(|&&(name, _)| name == class)
            .map(|&(_, shape)| shape)
    }
}

/// The shapes whose values are `Instance`s of a built-in class, by interned
/// class name: a compare of two ids, not of two strings.
fn instance_shapes() -> &'static [(Symbol, DispatchShape); 10] {
    static SHAPES: OnceLock<[(Symbol, DispatchShape); 10]> = OnceLock::new();
    SHAPES.get_or_init(|| {
        [
            (Symbol::intern("Date"), DispatchShape::Date),
            (Symbol::intern("DateTime"), DispatchShape::DateTime),
            (Symbol::intern("Instant"), DispatchShape::Instant),
            (Symbol::intern("Duration"), DispatchShape::Duration),
            (Symbol::intern("Match"), DispatchShape::Match),
            (Symbol::intern("IO::Path"), DispatchShape::IoPath),
            (Symbol::intern("IO::Path::Unix"), DispatchShape::IoPath),
            (Symbol::intern("IO::Path::Win32"), DispatchShape::IoPath),
            (Symbol::intern("IO::Path::Cygwin"), DispatchShape::IoPath),
            (Symbol::intern("IO::Path::QNX"), DispatchShape::IoPath),
        ]
    })
}
