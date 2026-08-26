# Method literals now accept an explicit invocant declaration

A `method (...)` literal may spell its invocant out: `method ($x: $p) { ... }` names it, and
`method (List:D:) { ... }` only constrains its type. Both forms were broken in mutsu — the named
one died with `Too few positionals passed; expected 3 arguments but got 2`, and the same shape
reached through `.^add_method` died at runtime with `Variable '$x' is not declared`. The
`my method (List:D:) { ... }` term form was a hard parse error (`Confused. expected statement`).

## Root cause

mutsu carries a method literal's receiver in a leading synthetic `self` parameter, because the
invocant arrives at the closure binder as the first *positional* argument (unlike a class method,
whose binder pulls the invocant off the dispatch frame). `parse_anon_method_with_params`
(`src/parser/primary/ident/anon_sub.rs`) prepended that `self` parameter **unconditionally**, on
top of whatever the signature declared. A signature that declared its own invocant therefore ended
up with *two* invocant parameters — `(self, x, p)` for `method ($x: $p)` — so every positional was
off by one and `$x` consumed the first real argument instead of aliasing the receiver. With no
argument left over, the binder reported the missing positional. In the `.^add_method` shape the
extra parameter was simply never bound, which surfaced later as `Variable '$x' is not declared`.

The `my method (...)` parse error was a separate gap in the same feature: the `my`/`our`/`state`
expression-term arm in `src/parser/primary/ident/identifier_call.rs` fell back to the anonymous
*package* parsers (`my class`, `my role`, `my grammar`) but had no anonymous-*method* fallback, so
`$obj.&(my method (List:D:) { ... })` never reached the method-literal parser at all.

A third, smaller bug showed up once the first two were fixed: `self` inside such a method came back
itemized (`$("a", "b", "c")` instead of `("a", "b", "c")`). `itemize_plain_scalar_param`
(`src/vm/vm_helpers.rs`) treats any parameter without an `@`/`%`/`&` sigil as a `$`-scalar item
binding, and the synthetic `self` has no sigil. Rakudo binds the invocant raw.

## Fix

`parse_anon_method_with_params` now folds a declared invocant into the single `self` parameter
rather than appending to it: the declaration's type and `where` constraints move onto `self` (so
`method (List:D:)` still type-checks the receiver, and `method (Int:D:)` still rejects a `Str`), and
a user-chosen name is bound in the body with a prologue `my $x := self;`. Both `self` and `$x` are
therefore live, exactly as in rakudo. The `my`/`our`/`state` term arm gained an anonymous-method
fallback for the `(...)` and `{ ... }` forms, and `itemize_plain_scalar_param` now exempts invocant
parameters.

Pinned by `t/multi-dispatch-ordering.t` (twelve subtests covering named, typed-named, type-only and
implicit invocants, the `my method` term form, and raw invocant binding).
