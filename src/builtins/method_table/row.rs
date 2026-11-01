//! The row type: what a built-in method is (ADR-11276 §2).

use super::ReceiverPlace;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

/// A [`Handler::Narrow`] implementation.
pub(crate) type NarrowFn = fn(&Value, &[Value]) -> Option<Result<Value, RuntimeError>>;

/// A [`Handler::Named`] implementation: the positional arguments, then the
/// named ones the row declared.
pub(crate) type NamedFn = fn(&Value, &[Value], Named<'_>) -> Option<Result<Value, RuntimeError>>;

/// A [`Handler::Interp`] implementation.
pub(crate) type InterpFn =
    fn(&mut Interpreter, &Value, &[Value], Named<'_>) -> Option<Result<Value, RuntimeError>>;

/// A [`Handler::Mut`] implementation: the receiver's place, the positional
/// arguments, then the named ones the row declared. `None` declines, and the
/// call takes the cascades.
pub(crate) type MutFn = fn(
    &mut Interpreter,
    &mut ReceiverPlace<'_>,
    &[Value],
    Named<'_>,
) -> Option<Result<Value, RuntimeError>>;

/// A built-in method's implementation.
#[derive(Clone, Copy)]
pub(crate) enum Handler {
    /// Needs no interpreter. `args` are the positional arguments, exactly
    /// [`MethodRow::arity`] of them, each a plain scalar (or, for a row
    /// flagged [`RowFlags::ANY_ARGS`], any argument the guard admits).
    Pure(fn(&Value, &[Value]) -> Result<Value, RuntimeError>),
    /// A [`Self::Pure`] handler whose row binds only some argument values:
    /// `None` means these arguments are outside the row's signature, and the
    /// call takes the cascades (the way a multi candidate fails to bind).
    Narrow(NarrowFn),
    /// A [`Self::Narrow`] handler that also reads named arguments. The row's
    /// [`MethodRow::named`] lists the names it binds; a call passing any
    /// other name takes the cascades, and the handler sees the rest as
    /// [`Named`].
    Named(NamedFn),
    /// A handler that needs the interpreter: it calls closures, reads
    /// dynamic variables or does I/O. Reached only from the entries that
    /// have an interpreter (`Interpreter::try_native_method`, the VM's
    /// call-site lane and `call_method_with_values`); the pure entries
    /// (`value::gist`, constant folding, the cascades' own prologue) skip it.
    /// Its effects make the debug cross-check unable to re-run it, so it is
    /// answered once, by the row.
    Interp(InterpFn),
    /// A handler that writes through its receiver (`push`, `splice`,
    /// `subst-mutate`, `BagHash.add`): it gets the receiver's
    /// [`ReceiverPlace`] and needs the interpreter. A `Mut` row is registered
    /// by its owner only, never by a receiver shape, so no shape lookup, no
    /// call-site lane and no pure entry reaches it; the one entry is
    /// [`invoke_mut`](super::invoke_mut). The debug cross-check never re-runs
    /// it, because a second run would apply the mutation twice.
    Mut(MutFn),
}

impl Handler {
    /// Whether the handler can be run without an interpreter.
    // Cost: O(1).
    pub(crate) const fn is_pure(self) -> bool {
        !matches!(self, Handler::Interp(_) | Handler::Mut(_))
    }

    /// Whether the handler writes through its receiver, so that only
    /// [`invoke_mut`](super::invoke_mut) may call it.
    // Cost: O(1).
    pub(crate) const fn is_mut(self) -> bool {
        matches!(self, Handler::Mut(_))
    }
}

/// Facts about a row that the guard step reads before it calls the handler
/// (ADR-11276 §2, `RowFlags`).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) struct RowFlags(u32);

impl RowFlags {
    /// No flag: the row answers a defined receiver of a plain shape, and its
    /// arguments must be plain scalars.
    pub(crate) const NONE: RowFlags = RowFlags(0);
    /// The row also answers a type object (`Str.gist`), not only a defined
    /// instance. The guard refuses a type object receiver for every other
    /// row, because most methods fail on an undefined invocant in their own
    /// way.
    pub(crate) const TYPE_OBJECT_OK: RowFlags = RowFlags(1 << 0);
    /// The handler binds any positional argument the guard admits
    /// (`Value::is_plain_argument`), not only the plain scalars. The guard
    /// still refuses what needs a probe the table skips: a `Junction` (which
    /// must autothread), a `Failure` (which may explode under `use fatal`)
    /// and a deferred `Seq` (which must be reified) take the cascades.
    pub(crate) const ANY_ARGS: RowFlags = RowFlags(1 << 1);
    /// The answer differs between two runs by design: it is random (`rand`,
    /// `pick`, `roll`), or it is a new object with an identity of its own
    /// (`List.Supply` mints a `Supply` instance). The debug cross-check cannot
    /// re-run the call through the cascades and compare. The handler is still
    /// pure.
    pub(crate) const RANDOM: RowFlags = RowFlags(1 << 2);
    /// The method is slurpy (`*@parts`): the row's arity is the fewest
    /// positional arguments it takes, and it binds any number above that. The
    /// table answers it for every arity from its own up to the largest one a
    /// call-site lookup carries (7); a longer call reaches it through the
    /// owner lookup (`invoke_owner`).
    pub(crate) const SLURPY: RowFlags = RowFlags(1 << 3);
    /// The row is reached only through its owner (`invoke_owner`), never by a
    /// receiver's shape: either the class has no shape (`IO::Special`, `Thread`)
    /// or the method writes its result back over the receiver
    /// (`IO::Handle.open` answers `self` with the opened handle's state), which
    /// only the mutating dispatch entries do; a shape lookup would run it on a
    /// copy. Slice 3F's `Handler::Mut` retires the flag.
    pub(crate) const OWNER_ONLY: RowFlags = RowFlags(1 << 4);
    /// The row binds every named argument the call passes (`|c`, `*%_`), so the
    /// guard step hands them all to the handler as [`Named`] instead of
    /// dropping the ones the row does not list (`Str.subst-mutate`, whose
    /// adverbs `:g`, `:i`, `:x`, ... are the pattern's, not the method's).
    pub(crate) const ANY_NAMED: RowFlags = RowFlags(1 << 5);
    /// The row is the *base candidate* of a user override's deferral chain
    /// (`Mu.new`, `Mu.clone`, `Mu.BUILDALL`, `Mu.POPULATE`, ADR-11276 §9.47):
    /// the last link a `callsame`/`nextsame` reaches after every user method.
    /// It is found only by [`base_row`](super::base_row), never by a shape or by
    /// an owner chain (`owner_row`): a receiver whose owners include `Mu`
    /// (`IO::Special`'s `["IO::Special", "Mu"]`) must not have its own `new` or
    /// `clone` answered by it. The handler gets the receiver first among its
    /// arguments.
    pub(crate) const DEFERRAL_BASE: RowFlags = RowFlags(1 << 6);
    /// The answer is a lazy view of the live receiver (`Array.batch` steps a
    /// cursor over the array, so a later `push` shows in the batches). The
    /// debug cross-check renders both answers, which reifies the view and
    /// would end the very liveness the row exists to keep, so it skips the
    /// row. The handler is still pure.
    pub(crate) const LIVE_VIEW: RowFlags = RowFlags(1 << 7);
    /// The row's one positional argument is a positional index, which it
    /// takes only as an `Int` (Rakudo's `Int:D \pos` candidate). The guard
    /// step admits nothing else; every other argument (a `Num`, a `Str`, a
    /// collection, a user object, a type object) is bound by
    /// [`index_arg`](super::index_arg) the way Rakudo's other candidates bind
    /// it: `Any.AT-POS(Any:D \pos)` re-dispatches on `pos.Int`,
    /// `Any.EXISTS-POS(Any:D \pos)` is `pos.Int == 0`, and a type object
    /// cannot be an index. The `.Int` call is user code for a user object, so
    /// only the interpreter's entry binds it.
    pub(crate) const INDEX_ARG: RowFlags = RowFlags(1 << 8);
    /// With [`Self::INDEX_ARG`]: the index parameter is `Int(Any)` (Rakudo's
    /// `Capture`), so every argument, a type object included, is coerced
    /// through its `.Int` and handed to the row.
    pub(crate) const INDEX_COERCES: RowFlags = RowFlags(1 << 9);
    /// The row binds `Mu \topic`: any argument, a user object included, which
    /// its [`Handler::Interp`] handler reads through the object's own methods
    /// (`Pair.ACCEPTS` calls the method its key names). The guard step hands it
    /// the arguments [`Self::ANY_ARGS`] admits; `invoke_unadmitted` hands it
    /// every other one but a `Junction`, a `Failure` and a named argument.
    pub(crate) const OBJECT_ARGS: RowFlags = RowFlags(1 << 10);
    /// The row's parameters are `Mu`: a `Junction` argument binds as itself
    /// (`grep(any(...))` matches against the junction) instead of
    /// autothreading the call. Only the MRO walk (`invoke_mro`) reads it; the
    /// guard step still leaves a `Junction` to the cascades.
    pub(crate) const JUNCTION_ARGS: RowFlags = RowFlags(1 << 11);
    /// The method exists only in language revision 6.e (Rakudo declares it
    /// in its `core.e` setting: `Any.snip`, `Str.nomark`, `IO::Path.stem`,
    /// ADR-11276 §9.146). Under an older revision every owner lookup skips
    /// the row, so the call finds no method, as in Rakudo. The shape tables
    /// are static, so such a row is [`Self::OWNER_ONLY`].
    pub(crate) const REVISION_E: RowFlags = RowFlags(1 << 12);
    /// The invocant is `:D` (Rakudo's `method abs(Int:D:)`): a type object
    /// that reaches the row is refused with `X::Parameter::InvalidConcreteness`
    /// naming the row's owner, instead of the call going on to another
    /// candidate (`Int.abs`, `IO::Path.e`). Without the flag a type object
    /// skips the row. Implies nothing for an instance.
    pub(crate) const DEFINITE: RowFlags = RowFlags(1 << 13);
    /// Every candidate of the method is `:D` (`multi method sqrt(Int:D:)`): a
    /// type object that reaches the row is refused with `X::Multi::NoMatch`,
    /// as Rakudo's multi dispatch finds no candidate (`Int.sqrt`).
    pub(crate) const DEFINITE_MULTI: RowFlags = RowFlags(1 << 14);
    /// The owner is immutable and the method refuses every receiver, a type
    /// object included, with `X::Immutable` (`List.push`).
    pub(crate) const IMMUTABLE: RowFlags = RowFlags(1 << 15);
    /// A positional argument is a number the method reads as one (Rakudo's
    /// `Num(Real) $base`, `Numeric $x`): a user object the guard step
    /// declines is coerced first, through its `Bridge` when its class composes
    /// `Real` and its `Numeric` otherwise, and the row is handed the number
    /// (`invoke_unadmitted`; `Num.log($real-object)`, ADR-11276 §9.184).
    pub(crate) const NUMERIC_ARGS: RowFlags = RowFlags(1 << 16);
    /// A positional argument may be a raw VM array (`nqp::list_s(...)`, a
    /// `BOOTStrArray`), which is not a plain argument by default. The handler
    /// reads its elements as the array they are (`Rakudo::Sorting.MERGESORT-str`).
    pub(crate) const VM_ARRAY_ARGS: RowFlags = RowFlags(1 << 17);
    /// The handler needs no interpreter but changes state outside its answer:
    /// it edits the receiver's shared attribute cell (a decoder's byte queue,
    /// a `Pod::Block::Declarator`'s text) or the runtime's tables (a
    /// cancellation, a condition variable). No debug cross-check may run its
    /// call a second time, and the call-site lane treats it as it treats an
    /// interpreter row.
    pub(crate) const EFFECTS: RowFlags = RowFlags(1 << 18);
    /// The row is a class method of a built-in type, called on the type object
    /// of the owner or of a user subclass of it (`Promise.start`,
    /// `Supply.from-list`, `List.from-iterator`). Every owner with such a row
    /// is a class-method owner ([`class_method_owner`](super::class_method_owner)),
    /// which the by-name dispatch asks once for a type object.
    pub(crate) const CLASS_METHOD: RowFlags = RowFlags(1 << 19);
    /// The named arguments are a raw capture relayed to another binder
    /// (`Mu.new`/`bless`), rather than ordinary named parameter values.
    pub(crate) const RAW_NAMED: RowFlags = RowFlags(1 << 20);

    /// The flags of both.
    // Cost: O(1).
    pub(crate) const fn or(self, other: RowFlags) -> RowFlags {
        RowFlags(self.0 | other.0)
    }

    /// Whether the debug cross-checks cannot run this row's call a second
    /// time and compare the rendered answers: a random one ([`Self::RANDOM`]),
    /// a lazy view of the receiver ([`Self::LIVE_VIEW`]), one of language
    /// revision 6.e ([`Self::REVISION_E`]) or one with effects
    /// ([`Self::EFFECTS`]).
    // Cost: O(1).
    pub(crate) const fn skips_cross_check(self) -> bool {
        // The cascades know no language revision: a `REVISION_E` row's answer
        // is not theirs.
        self.0 & (Self::RANDOM.0 | Self::LIVE_VIEW.0 | Self::REVISION_E.0 | Self::EFFECTS.0) != 0
    }

    /// Whether the row exists in the program's language revision: always,
    /// unless it is a [`Self::REVISION_E`] row and the revision is older.
    // Cost: O(1) (a thread-local read for a `REVISION_E` row).
    #[inline]
    pub(crate) fn in_force(self) -> bool {
        !self.contains(Self::REVISION_E)
            || crate::parser::current_language_version_starts_with("6.e")
    }

    /// Whether every bit of `flag` is set.
    // Cost: O(1).
    pub(crate) const fn contains(self, flag: RowFlags) -> bool {
        self.0 & flag.0 == flag.0
    }
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
    pub(crate) flags: RowFlags,
    /// The named arguments the row binds. A call passing a name no row of the
    /// method binds has it swallowed (the implicit `*%_` of a method); one
    /// that another row of the method binds is that row's, so this one
    /// declines (`dispatch::bind_named`).
    pub(crate) named: &'static [&'static str],
}

impl MethodRow {
    /// Bind ordinary named parameters by value; capture relays keep cells.
    // Cost: O(n + k), n = named arguments, k = copied key bytes. Rakudo: O(n) -- see #9252.
    pub(super) fn bind_named<'a>(&self, args: &'a [Value]) -> std::borrow::Cow<'a, [Value]> {
        if self.flags.contains(RowFlags::RAW_NAMED) {
            std::borrow::Cow::Borrowed(args)
        } else {
            crate::value::call_args::read_named_args(args)
        }
    }

    /// The arities the table registers the row at: its own, or for a slurpy
    /// row every one from its own up to 7.
    // Cost: O(1).
    pub(crate) fn arities(&self) -> std::ops::RangeInclusive<u8> {
        if self.flags.contains(RowFlags::SLURPY) {
            self.arity..=7
        } else {
            self.arity..=self.arity
        }
    }

    /// The error the type object `type_name` gets from this row, when its
    /// invocant is `:D` ([`RowFlags::DEFINITE`], [`RowFlags::DEFINITE_MULTI`]);
    /// `None` when the row says nothing about a type object, so the call goes
    /// on to the next candidate.
    // Cost: O(n), n = chars of the message.
    pub(crate) fn type_object_refusal(&self, type_name: &str) -> Option<RuntimeError> {
        if self.flags.contains(RowFlags::DEFINITE) {
            Some(RuntimeError::parameter_invalid_concreteness(
                self.owner, type_name, self.name, "self", true, true,
            ))
        } else if self.flags.contains(RowFlags::IMMUTABLE) {
            Some(
                crate::runtime::methods_signature_errors::make_x_immutable_error(
                    self.name, self.owner,
                ),
            )
        } else if self.flags.contains(RowFlags::DEFINITE_MULTI) {
            Some(
                crate::runtime::methods_signature_errors::make_multi_no_match_error_detailed(
                    self.name,
                    type_name,
                    false,
                    "",
                    &[],
                ),
            )
        } else {
            None
        }
    }
}

/// The named arguments of one call, split from its positional ones.
///
/// Whether an argument is named is a property of the call site, carried by
/// the value's flavour (ADR-0021): a string-keyed `Pair` is named, a
/// `ValuePair` is a positional `Pair`. The guard step splits them once, so a
/// handler never sees a named argument among its positionals.
#[derive(Clone, Copy)]
pub(crate) struct Named<'a>(&'a [Value]);

impl<'a> Named<'a> {
    /// A call with no named argument.
    pub(crate) const NONE: Named<'static> = Named(&[]);

    /// A view over arguments that are all string-keyed `Pair`s.
    // Cost: O(1).
    pub(super) fn new(pairs: &'a [Value]) -> Self {
        Named(pairs)
    }

    /// Bound named arguments as string-keyed `Pair`s, for a primitive that
    /// reads them as one list. Raw capture relays retain the original cells.
    // Cost: O(1).
    pub(crate) fn pairs(&self) -> &'a [Value] {
        self.0
    }

    /// The value of the named argument `name`, if the call passed it.
    // Cost: O(n), n = named arguments of the call.
    pub(crate) fn get(&self, name: &str) -> Option<&'a Value> {
        self.0.iter().find_map(|arg| match arg.view() {
            ValueView::Pair(key, value) if key == name => Some(value),
            _ => None,
        })
    }
}
