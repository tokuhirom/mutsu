# `sprintf`'s numeric directives warn for a bare type object in numeric context

A bare type object rendered by a numeric directive (`%d %i %u %b %o %x %X %c
%e %f %g`) now emits rakudo's `Use of uninitialized value of type X in numeric
context` warning, matching `+Int`. The rendered value is unchanged — the type
object still coerces to `0` (`"0"`, `"0.000000"`, etc.) — this only adds the
missing warning, completing the sprintf type-object story alongside the earlier
`%s` string-context fix.

```raku
sprintf("%d", Int)   # "0" + numeric-context warning (was: "0" silently)
sprintf("%f", Int)   # "0.000000" + warning
```

## Implementation

A new `Interpreter::warn_type_object_numeric_context` helper mirrors the
string-context sibling: it raises the resumable numeric warning (no
`Methods .^name...` suffix — that is string-context only) and resumes with
integer `0`, reconciling any captured-outer writeback from the warning handler.

`builtin_sprintf`'s per-directive loop already isolated the bare-type-object /
no-user-method case for `%s`; its numeric branch now calls the new helper. The
pure formatter already coerces a type object to the correct numeric `0`, so the
numeric arm only emits the warning without substituting the value. A
user-defined `.Int` / `.Numeric` still dispatches without warning.

Pin: `t/sprintf-type-object-numeric-context.t`.
