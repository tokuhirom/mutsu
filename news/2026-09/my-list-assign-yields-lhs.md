# `my ($x, $y) = RHS` evaluates to its LHS, so an infinite RHS no longer dies when sunk

A declaring list assignment is desugared by the parser into a synthetic block:
the RHS is staged in `@__destructure_tmp__`, one declaration per target reads
its element, and a trailing expression gives the block its value. That trailing
expression was the staging temp itself -- i.e. the whole RHS. So
`(my ($x, $y) = 1, 2, 3)` was `$[1, 2, 3]` where Rakudo gives `$(1, 2)`
(`List.STORE` returns its invocant, the LHS containers).

Since #9331 turned `xx *`, `X` and `Z` into real lazy streams, the wrong value
became fatal: any non-final `my ($x, $y) = <infinite RHS>;` statement sank the
infinite list, and `SinkPop` forced it -- `Cannot coerce an infinite lazy list
to a strict list`, or a hang for `(1..*).map(...)`.

In assignment mode the block now ends in a list of the declared targets
(`$`/`@`/`%`/`&`/sigilless; a literal postconstraint element yields its staged
value), so the value is the LHS after assignment and holds the real containers
(`$r[0] = 9` writes `$x`, as in Rakudo). Binding mode (`:=`) is unchanged. The
sink-warning pass now recognises the whole destructuring block and walks only
its statement prefix, instead of special-casing the temp's name, so neither the
new target list nor the binding-mode temp triggers a spurious "Useless use".

Pinned by `t/collections/my-list-assign-result.t`. Closes #9342.
