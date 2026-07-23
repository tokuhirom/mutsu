# Bare type objects warn in string context

Using a bare type object (an undefined value such as `Int`, `Str`, or an
uninitialized `my Int $x`) in a string context now emits Rakudo's

```
Use of uninitialized value of type Int in string context.
Methods .^name, .raku, .gist, or .say can be used to stringify it to something meaningful.
```

warning and resumes with the empty string, matching raku. Previously mutsu
silently resumed with `""` from most string-context entry points, warning only
from `print` and the `.Str` method.

The warning is now emitted from every string-context entry point a type object
can reach:

- prefix `~` (`~Int`) — `src/vm/vm_misc_coerce.rs`
- infix `~` and the string comparators `eq`/`lt`/… (`"a" ~ Int`, `Int eq "x"`)
  — `src/vm/vm_coerce_concat_ops.rs::coerce_stringy_operand`
- string interpolation (`"$x"` with an uninitialized typed `$x`, `"{Int}"`) —
  `src/vm/vm_var_assign_typed.rs::exec_string_concat_op`
- `print` — `src/runtime/io_env.rs::render_str_value`

A shared helper `warn_type_object_string_context` (`src/runtime/io_env.rs`)
formats the message and reconciles any captured-outer writeback from the
resumable-warning handler.

A class that defines a user `.Str`/`.Stringy` still dispatches it even on the
bare type object (`class A { method Str {"foo"} }` then `~A` / `"{A}"` /
`print A` all render `"foo"`), matching raku — the warning fires only for type
objects with no user stringifier. This also fixed a pre-existing `print` bug
where a type object with a custom `.Str` warned instead of dispatching it.

`Mu` keeps its existing behavior: prefix `~Mu` is the hard error
`Cannot resolve caller prefix:<~>(Mu:U)`, while infix `~`/interpolation/`print`
warn like any other type object.

Two known minor divergences from raku remain and are not addressed here:

- Block interpolation `"{Int}"` emits the `...value element of type...` wording
  (the same as variable interpolation) rather than raku's `...value of type...`.
  Distinguishing the two forms would require the compiler to tag each
  interpolation part as a variable vs an expression at the `StringConcat` op;
  the common `$var`/`$(...)` form matches raku exactly.
- `Mu eq "x"` warns and returns `False` instead of raku's
  `Cannot resolve caller infix:<eq>(Mu:U, Str:D)` hard error.

Pinned by `t/type-object-str-context-warning.t`.
