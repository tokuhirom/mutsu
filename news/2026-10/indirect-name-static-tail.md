# Indirect names take a static tail: `::($n)::Bar`, `::($n)::`

An indirect name `::(EXPR)` can now be followed by static segments and/or a
trailing `::`. mutsu's term parser used to stop right after the `)`, so
`::($n)::Bar` died with "Confused. Two terms in a row". That parse error was
the first failure in `S02-names/pseudo-6d.t`, `pseudo-6e.t` and
`6.c/S02-names/pseudo-6c.t`, all on `::($our)::A43.WHO`.

- `::($n)::Bar` and `::("A")::B::C` look up `EXPR ~ "::Bar"`, the same symbol
  `::("$n\::Bar")` would find.
- `::($n)::` names the package itself, not its stash. That is what Rakudo
  2026.09 does: `::($n)::.keys` is `()`, and `::($n)::<$v>` subscripts the
  package and gives `(Any)`.
- A following dynamic segment `::($n)::('Bar')` still goes through the
  existing stash road, and `$::($our)::A::x` is unchanged.

The parser keeps the parts apart in a new `Expr::IndirectTypeLookupTail`. The
compiler emits exactly the existing `IndirectTypeLookup` over the concatenated
name, so there is no new runtime path. Keeping the parts also lets RakuAST
render Rakudo's shape, `Name.new(Part::Empty.new, Part::Expression(...),
Part::Simple("Bar"))` with a closing `Part::Empty` for a trailing `::`, and
lower it back.

Regression test: `t/vm/binding/indirect-name-static-tail.t` (GH #10657). Now
that the roast files run further, the next blocker in `pseudo-6d.t` is a
debug-build invariant panic on `$OUR::x30 := $x`, filed as #10731.
