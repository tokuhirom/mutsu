//! The row type: what a built-in method is (ADR-11276 §2).

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

/// A built-in method's implementation.
///
/// ADR-11276 §2 also names a receiver-writing kind (`Mut`); slice 3F adds it
/// together with `ReceiverPlace`.
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
}

impl Handler {
    /// Whether the handler can be run without an interpreter.
    // Cost: O(1).
    pub(crate) const fn is_pure(self) -> bool {
        !matches!(self, Handler::Interp(_))
    }
}

/// Facts about a row that the guard step reads before it calls the handler
/// (ADR-11276 §2, `RowFlags`).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) struct RowFlags(u8);

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
    /// The answer is random (`rand`, `pick`, `roll`), so the debug cross-check
    /// cannot re-run the call through the cascades and compare: two runs
    /// differ by design. The handler is still pure.
    pub(crate) const RANDOM: RowFlags = RowFlags(1 << 2);

    /// The flags of both.
    // Cost: O(1).
    pub(crate) const fn or(self, other: RowFlags) -> RowFlags {
        RowFlags(self.0 | other.0)
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
    /// The named arguments the row binds. A call passing a name that is not
    /// listed takes the cascades, which apply the implicit `*%_` of a
    /// method (`accepted_nameds`).
    pub(crate) named: &'static [&'static str],
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

    /// The value of the named argument `name`, if the call passed it.
    // Cost: O(n), n = named arguments of the call.
    pub(crate) fn get(&self, name: &str) -> Option<&'a Value> {
        self.0.iter().find_map(|arg| match arg.view() {
            ValueView::Pair(key, value) if key == name => Some(value),
            _ => None,
        })
    }
}
