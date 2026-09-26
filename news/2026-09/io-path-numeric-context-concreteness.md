# IO::Path in numeric context dies instead of warning

`IO::Path + 1`, `+IO::Path`, `-IO::Path` and `IO::Path * 2` used to warn
"Use of uninitialized value of type IO::Path in numeric context" and coerce
to `0`, the same generic fallback a plain `Any`-ish type object gets. Rakudo
instead raises `X::Parameter::InvalidConcreteness`, because `IO::Path`'s
`Numeric` method is declared `:D:` and has no candidate for the type object.

`#9623` had already fixed the explicit method call (`IO::Path.Numeric`), but
numeric *context* — the arithmetic infix operators, prefix `+`/`-`, and the
numeric comparisons — coerces a type object without going through method
dispatch, so it never consulted that check.

The fix shares the existing `IO_PATH_CONCRETE_METHODS` table
(`src/vm/vm_type_object_concreteness.rs`) with the numeric-context coercion
paths (`check_type_object_in_numeric_context`, used by every arithmetic and
comparison op; `exec_num_coerce_op` and `exec_negate_op`, used by prefix
`+`/`-`) instead of duplicating the "which built-in methods are `:D:`"
knowledge. A type object with no `:D:`-only `Numeric` (`Str`, `Any`, ...)
keeps warning and coercing to zero, unaffected.
