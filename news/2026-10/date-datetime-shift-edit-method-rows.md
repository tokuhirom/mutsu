# Date and DateTime shift/edit methods are rows in the method table

`later`, `earlier`, `truncated-to`, `in-timezone` and `DateTime.local` are rows
of the built-in method table now (ADR-11276 §9.34), with `local` the one row
that needs the interpreter (it reads `$*TZ`). The 680-line
`runtime/methods_temporal.rs` is split into two modules next to the other
`Date`/`DateTime` rows, and the cascade a subclass instance still takes calls
the same functions, so there is one implementation of each method.

No behaviour change: `t/oo/method/temporal-shift-edit-method-rows.t` pins the
answers, each compared with Rakudo, including a `Date`/`DateTime` subclass
keeping its class.
