# An element bound to a bare value refuses `++`, `--` and `[;]=`

An element `:=`-bound to a bare value (`@a[1] := 42`, `@a.BIND-POS(1, 42)`,
`%h.BIND-KEY("a", 1)`) has no container, but the in-place increment and
decrement ops wrote straight through it: `@b[1]++` turned the bound `42` into
`43`. They now die with the `X::Multi::NoMatch` Rakudo's rw-only
`postfix:<++>` candidates raise ("Cannot resolve caller postfix:<++>(Int:D);
the parameter requires mutable arguments"), checking both the name-keyed
read-only marker a literal `:=` records and the read-only cell
`.BIND-POS`/`.BIND-KEY` store.

Rebinding such an element to a variable (`@c[1] := 7; @c[1] := $x`) now drops
the read-only marker, so `@c[1] = 6` writes `$x` instead of dying, and the
multi-dimensional `[;]=` store refuses a slot a multi-index `BIND-POS` bound to
a value (`X::Assignment::RO` for a shaped array, `X::AdHoc` for a nested one,
as in Rakudo). The three copies of the `++`/`--` "requires mutable arguments"
error now share one constructor (#10984).
