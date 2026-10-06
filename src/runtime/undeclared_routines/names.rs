//! The static name tables the undeclared-routine check consults.

/// Core *term* constants: names that resolve to a value in CORE but are not
/// routines. Calling one of them is not the same error as calling a name
/// nobody declared — the symbol exists, it just does not exist under the `&`
/// sigil — so rakudo answers `X::Undeclared` naming `&e` ("Variable '&e' is
/// not declared") where an entirely unknown `zzz()` gets the CHECK-time
/// `X::Undeclared::Symbols`. The two classes are unrelated (`X::Comp` is a
/// role, not a superclass), so `throws-like 'e()', X::Undeclared` sees the
/// difference.
///
/// `now`, `time` and `rand` are deliberately absent: they are real routines.
pub(crate) const CORE_TERM_CONSTANTS: &[&str] =
    &["e", "i", "pi", "tau", "Inf", "NaN", "True", "False"];

/// Native (lowercase) type names that may appear in call position as
/// coercions/constructors (`int8(...)`) without being routine declarations.
pub(super) const NATIVE_TYPE_NAMES: &[&str] = &[
    "int",
    "int8",
    "int16",
    "int32",
    "int64",
    "uint",
    "uint8",
    "uint16",
    "uint32",
    "uint64",
    "num",
    "num32",
    "num64",
    "str",
    "bool",
    "byte",
    "atomicint",
    "complex",
    "size_t",
    "ssize_t",
    "long",
    "ulong",
    "longlong",
    "ulonglong",
    "buf8",
    "buf16",
    "buf32",
    "buf64",
    "blob8",
    "blob16",
    "blob32",
    "blob64",
    "utf8",
    "array",
];

/// Callables the compiler special-cases into dedicated opcodes, so they never
/// reach the runtime function-dispatch tables (`is_builtin_function` /
/// `EVAL_KNOWN_ROUTINE_NAMES` don't list them all).
pub(super) const COMPILER_SPECIAL_CALL_NAMES: &[&str] = &[
    "cas",
    "atomic-assign",
    "atomic-fetch",
    "atomic-fetch-add",
    "atomic-add-fetch",
    "atomic-fetch-sub",
    "atomic-sub-fetch",
    "atomic-fetch-inc",
    "atomic-inc-fetch",
    "atomic-fetch-dec",
    "atomic-dec-fetch",
    "done",
    "temp",
];

/// Phaser names, offered as suggestions for a lowercase typo (`begin` →
/// "Did you mean 'BEGIN'?", matching rakudo).
pub(crate) const PHASER_SUGGESTION_NAMES: &[&str] = &[
    "BEGIN", "CHECK", "INIT", "END", "ENTER", "LEAVE", "KEEP", "UNDO", "FIRST", "NEXT", "LAST",
    "PRE", "POST", "QUIT", "CLOSE",
];
