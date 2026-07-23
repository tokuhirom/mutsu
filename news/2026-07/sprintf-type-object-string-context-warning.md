# `sprintf`'s `%s` coerces a bare type object to "" with a string-context warning

A bare type object rendered by a `%s` directive now stringifies to the empty
string with rakudo's `Use of uninitialized value of type X in string context.`
warning, instead of the mutsu-specific `(Int)` gist. This matches how `~Int` and
`Int.Str` already behave, and how raku renders `sprintf("%s", Int)` (empty
string plus the warning). Numeric directives are unchanged — they already coerce
a type object to `0`.

Both call shapes agree now:

```raku
sprintf("%s", Int)        # "" + "Use of uninitialized value of type Int ..."
"%s".sprintf(Int)         # "" + same warning
sprintf("%s|%s", Int, 5)  # "|5"
```

## Implementation

The pure native formatter (`format_sprintf_impl`) has no interpreter context and
cannot emit warnings, so the type-object case is routed through the
interpreter-aware `Interpreter::builtin_sprintf`:

- `native_sprintf` (the VM fast path) now bails to `None` — falling back to
  `builtin_sprintf` — when any argument is a bare type object (`Package`), the
  same way it already defers `Instance`/`Junction` args.
- `builtin_sprintf` already maps each argument to its directive via
  `sprintf_arg_specs`; its per-argument loop now, for a bare type object with no
  user coercion method and a `%s` directive, emits
  `warn_type_object_string_context` and substitutes the empty string.
- The method form (`$fmt.sprintf(...)`) is unified onto the same path: the native
  1-arg `.sprintf` fast path defers a `Package` argument, and the slow-path
  `sprintf` method arm now delegates to `builtin_sprintf` (the single source of
  truth) rather than calling the pure formatter directly. A user-defined `.Str`
  on the type object still dispatches without warning.

Pin: `t/sprintf-type-object-str-context.t`.
