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
}

impl DispatchShape {
    /// Every shape, in declaration order. The call-site memo packs a shape
    /// into one byte and the table keeps a bit per shape, so this stays under
    /// 64.
    pub(crate) const ALL: [DispatchShape; 37] = [
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
        const_list_contains(&ROLES_WITH_ROWS, role)
    }

    /// [`Self::reaches`] for the rows of `owner` as a whole, before the
    /// per-method audit of `Cool` for the closed collection shapes.
    // Cost: O(1).
    pub(crate) const fn reaches_owner(self, owner: &str) -> bool {
        self.inherits()
            || const_str_eq(owner, self.type_name())
            // `IO::Handle` renders itself (`Mu.raku`) through a `Mu` row of its own.
            || (matches!(self, DispatchShape::IoHandle) && const_str_eq(owner, "Mu"))
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
            || (self.reaches_audited_any() && const_str_eq(owner, "Any"))
            || (!self.inherits() && const_str_eq(owner, "Mu"))
    }

    /// Whether `Mu`'s or `Any`'s row `method` reaches this shape through the
    /// per-method audit of a closed shape.
    // Cost: O(m), m = audited names.
    pub(crate) const fn reaches_audited_ancestor(self, method: &str) -> bool {
        self.reaches_audited("Mu", method) || self.reaches_audited("Any", method)
    }

    /// Whether this closed shape reaches some of `Any`'s list rows
    /// ([`ANY_LIST_FOR_CLOSED_SHAPES`]; [`Self::reaches_audited_any_method`]
    /// says which).
    // Cost: O(1).
    const fn reaches_audited_any(self) -> bool {
        !self.inherits() && self.has_instances()
    }

    /// Whether `Any`'s list row `method` reaches this closed shape. None
    /// reaches `sort`, whose handler reads only a list (a closed shape sorts
    /// through the interpreter), and only a `Pair` reaches `reverse`, `min`
    /// and `max`, whose handlers read a scalar, a hash, a list and a pair. A `Capture` flattens to itself
    /// (`methods_0arg`'s `Capture` block), and the
    /// quant hashes read as their list of pairs for `unique` first
    /// (`hashlike_receiver_as_pairs_list`), then reach the row as a `List`;
    /// the rest reach the row with themselves as invocant.
    // Cost: O(m), m = audited names.
    const fn reaches_audited_any_method(self, method: &str) -> bool {
        if !const_list_contains(&ANY_LIST_FOR_CLOSED_SHAPES, method) {
            return false;
        }
        match self {
            // A `Match` answers the others on its matched string (`methods_0arg`'s
            // `Match` delegation), but joins its positional captures.
            DispatchShape::Match => const_str_eq(method, "join"),
            _ if const_str_eq(method, "sort") => false,
            // `min`/`max` read a `Pair` as its one element; the other closed
            // shapes take the interpreter's extremum walk.
            _ if const_str_eq(method, "min") || const_str_eq(method, "max") => {
                matches!(self, DispatchShape::Pair)
            }
            DispatchShape::Pair => true,
            _ if const_str_eq(method, "reverse") => false,
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
            return const_list_contains(&MU_FOR_CLOSED_SHAPES, method);
        }
        if matches!(self, DispatchShape::Match) {
            return const_list_contains(&COOL_TEXT_FOR_CLOSED_SHAPES, method)
                || const_list_contains(&COOL_TEXT_FOR_MATCH, method);
        }
        const_list_contains(&COOL_TEXT_FOR_CLOSED_SHAPES, method)
            || (!matches!(self, DispatchShape::IoPath)
                && const_list_contains(&COOL_NUMERIC_FOR_COLLECTIONS, method))
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

/// The roles whose rows reach every shape whose type composes them
/// ([`DispatchShape::reaches_role`]). Their only rows are the element and key
/// type methods `of`/`keyof` (§9.86), which read nothing but the receiver's
/// type and its container descriptor; a role gaining another row is audited
/// for every composing shape first.
pub(crate) const ROLES_WITH_ROWS: [&str; 7] = [
    "Positional",
    "Associative",
    "Callable",
    "Baggy",
    "Setty",
    "Mixy",
    "QuantHash",
];

/// The `Cool` numeric methods a `Range` or `Seq` reaches
/// ([`DispatchShape::reaches_audited`]).
const COOL_NUMERIC_FOR_COLLECTIONS: [&str; 51] = [
    "sin", "cos", "tan", "sec", "cosec", "cotan", "sinh", "cosh", "tanh", "sech", "cosech",
    "cotanh", "asin", "acos", "atan", "asec", "acosec", "acotan", "asinh", "acosh", "atanh",
    "asech", "acosech", "acotanh", "exp", "log", "log2", "log10", "sqrt", "cis", "atan2", "roots",
    "unpolar", "abs", "sign", "floor", "ceiling", "truncate", "round", "is-prime", "conj", "chr",
    "rand", "int", "uint", "byte", "Complex", "UInt", "Version", "Real", "Num",
];

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

/// The `Any` list methods every closed shape with instances reaches
/// ([`DispatchShape::reaches_audited_any`]): Rakudo's `Any` declares them as
/// `self.list.METHOD`, and their handlers read any receiver as its list (a
/// `Pair` or a `Set` as one element or its pairs, a `Blob` as its bytes).
/// A shape whose own type declares one of them (`Seq.reverse`, `Range.flat`)
/// answers its own row first. `join` (`self.list.join`) reads a `Pair` as its
/// `.Str`, a `Capture` as its positionals and a quant hash as its pairs
/// (`method_table::collections::join`).
pub(crate) const ANY_LIST_FOR_CLOSED_SHAPES: [&str; 14] = [
    "flat", "sort", "unique", "repeated", "reverse", "end", "join", "pick", "roll", "minmax",
    "head", "tail", "min", "max",
];

/// The `Mu` methods every closed shape reaches ([`DispatchShape::reaches_audited`])
/// and every receiver with no shape (`method_table::answer_shapeless`): their
/// handlers read only the receiver's truth or its lazy flag, wrap it in an item
/// container, or sink it, which every value supports. A shape whose own type declares one (`Match.not`, `Range.is-lazy`)
/// answers its own row first.
pub(crate) const MU_FOR_CLOSED_SHAPES: [&str; 12] = [
    "so", "not", "is-lazy", "item", "sink", "self", "defined", "DEFINITE", "Bool", "WHICH",
    "WHERE", "clone",
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

/// The shapes whose values are `Instance`s of a built-in class, by interned
/// class name: a compare of two ids, not of two strings.
fn instance_shapes() -> &'static [(Symbol, DispatchShape); 34] {
    static SHAPES: OnceLock<[(Symbol, DispatchShape); 34]> = OnceLock::new();
    SHAPES.get_or_init(|| {
        [
            (Symbol::intern("Date"), DispatchShape::Date),
            (Symbol::intern("Backtrace"), DispatchShape::Backtrace),
            (
                Symbol::intern("Backtrace::Frame"),
                DispatchShape::BacktraceFrame,
            ),
            (Symbol::intern("DateTime"), DispatchShape::DateTime),
            (Symbol::intern("Instant"), DispatchShape::Instant),
            (Symbol::intern("Duration"), DispatchShape::Duration),
            (Symbol::intern("Match"), DispatchShape::Match),
            (Symbol::intern("IO::Path"), DispatchShape::IoPath),
            (Symbol::intern("IO::Path::Unix"), DispatchShape::IoPath),
            (Symbol::intern("IO::Path::Win32"), DispatchShape::IoPath),
            (Symbol::intern("IO::Path::Cygwin"), DispatchShape::IoPath),
            (Symbol::intern("IO::Path::QNX"), DispatchShape::IoPath),
            (Symbol::intern("IO::Handle"), DispatchShape::IoHandle),
            (Symbol::intern("Blob"), DispatchShape::Blob),
            (Symbol::intern("Buf"), DispatchShape::Buf),
            (Symbol::intern("utf8"), DispatchShape::Blob),
            (Symbol::intern("utf16"), DispatchShape::Blob),
            (Symbol::intern("utf32"), DispatchShape::Blob),
            (Symbol::intern("Blob[uint8]"), DispatchShape::Blob),
            (Symbol::intern("Blob[uint16]"), DispatchShape::Blob),
            (Symbol::intern("Blob[uint32]"), DispatchShape::Blob),
            (Symbol::intern("Blob[uint64]"), DispatchShape::Blob),
            (Symbol::intern("Blob[int8]"), DispatchShape::Blob),
            (Symbol::intern("Blob[int16]"), DispatchShape::Blob),
            (Symbol::intern("Blob[int32]"), DispatchShape::Blob),
            (Symbol::intern("Blob[int64]"), DispatchShape::Blob),
            (Symbol::intern("Buf[uint8]"), DispatchShape::Buf),
            (Symbol::intern("Buf[uint16]"), DispatchShape::Buf),
            (Symbol::intern("Buf[uint32]"), DispatchShape::Buf),
            (Symbol::intern("Buf[uint64]"), DispatchShape::Buf),
            (Symbol::intern("Buf[int8]"), DispatchShape::Buf),
            (Symbol::intern("Buf[int16]"), DispatchShape::Buf),
            (Symbol::intern("Buf[int32]"), DispatchShape::Buf),
            (Symbol::intern("Buf[int64]"), DispatchShape::Buf),
        ]
    })
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
