# 6.e `MY::` is the innermost pad, and `LEXICAL::` binds what it reads

Under `use v6.e.PREVIEW`, `MY::` names the innermost lexical pad and nothing else, as in
rakudo 2026.11. `$MY::x`, `MY::<$x>` or `MY::Name` of a symbol that pad does not declare is
an `X::NoSuchSymbol` Failure, and so is a miss in any 6.e pseudo-stash (6.c and 6.d keep
returning `Nil`). A pseudo-stash now records the language revision of the code that built it.
`$MY::x := ...` rebinds the pad's own `$x`, an `our` alias included.

`LEXICAL::` now behaves as the view of every visible lexical it is: `$LEXICAL::x` reads the
nearest `$x`, `$LEXICAL::A1::pies` the member of the lexical package `A1`, `LEXICAL::<$*k>`
searches the dynamic scope, and `$LEXICAL::z := $x`, `LEXICAL::<$z> := $y` and the root
stash's `::<$x> := $y` bind the variable itself. A nested block's `LEXICAL::` lists the
packages the enclosing scopes declare. The root stash `::` answers each pseudo-package by
name (`::<OUTERS>:exists`, `::.<MY>.WHO.<$l>`).

`$a =:= $b` of two named variables now compares the slots the names resolve to at the
comparison, so an inner `my $z` that shadows an outer one is no longer compared against the
outer.

`S02-names/pseudo-6e.t` went from 42 failures to 18 (#18742).
