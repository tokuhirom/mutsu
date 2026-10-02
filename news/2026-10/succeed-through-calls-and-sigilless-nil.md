# `succeed` unwinds through routine calls; `my \x = Nil` keeps its Nil

Two small fixes from the `Spanish` distribution (#10554).

**`succeed` propagates dynamically.** Every routine and closure call boundary
used to absorb a `succeed` raised inside it, so `succeed` called from a helper
sub inside a `when` block never left the caller's `given`, and
`sub g { succeed 42 }` merely returned 42. Raku installs a succeed handler only
on a block that lexically contains a `when`/`default`; everything else lets the
signal unwind to the caller, the way `last`/`next` reach a caller's loop. The
routine and closure body compilers now record that on the chunk
(`CompiledCode::succeed_passes_through`, from the existing `reaches_when` scan),
and the named-sub, method, light/fast and closure call paths let the signal
through when it is set. A `succeed` that reaches the mainline with no `when`
clause to leave is now the `X::ControlFlow` "succeed without when clause"
rakudo reports, instead of silently ending the program.

**Explicit `Nil` initializers.** `my \x = Nil` and `constant z = Nil` were
mistaken for the parser's synthesized "no initializer" default and re-seeded as
`Any` (the package-scoped constant was even treated as a bare `our`
redeclaration). Sigilless and `constant` declarations always carry a
user-written initializer, so they are now marked `__has_initializer` like
every other initialized declaration.

The third item of the report — `next |c` with a `Label` value — needs
first-class `Label` objects and was split into #10737.
