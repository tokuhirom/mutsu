# `my ::T $x` outside a signature is typed `NQPMu`, and `names.t` passes

A `::T` type-capture on a plain declaration binds nothing, and rakudo types such a
variable `NQPMu` (no `Mu` methods): `my ::foo $x; say "ok"` lives, `say $x` dies with
`X::Method::NotFound` for `gist`, and `my ::foo $x = 5` fails the assignment type
check. mutsu dropped the constraint (`Any`), so `say $x` printed `(Any)`. The
declaration now keeps an `NQPMu` constraint; `say`/`put`/`print`/`note` of the type
object and any named method call on it (other than `WHAT`/`HOW`/`WHO`/`VAR`/`defined`
and `^` meta methods) raise `X::Method::NotFound`. With #16360 this completes
`roast/S02-names-vars/names.t`, which is now whitelisted. Part of #15516.
