# A block's own `multi infix:<op>` no longer leaks into the routines it calls

A `multi infix:<+>(Box $l, Int $r)` declared in a block used to be registered
in the flat routine registry for the block's dynamic extent, so a routine
written outside the block that the block called saw the candidate too:
`sub f { Box + 1 }; { multi infix:<+>(Box $l, Int $r) { "local" }; f() }`
gave `local` where rakudo warns and gives `1`.

A block's own `multi` of an operator whose ops rank user candidates against
the core ones (the arithmetic, comparison and string operators) now binds in
the block's lexical operator set, as a plain `sub infix:<op>` does since
#17771. The family it binds is the block's candidates plus those in scope
around the block: an enclosing block's family, else the unit's own and
imported candidates, with the built-in as the fallback. `&infix:<+>.candidates`
in the block lists the setting's candidates before the block's.

Fixed on the way:

- A string interpolation (`"A: $a"`) folds its pieces through the `~`
  candidates a block declares or imports; its op had no lexical prefix.
- A `.map` callback recompiled from its AST lost the block's sets when its
  call sites read different chains (`Thing` read the unit's `use` set, `+`
  did not, since the block's `multi` hides that import): the sets around the
  closure that any of its call sites reads are seeded now.
- A built-in fallback of such a family no longer calls the family again when
  the candidate's own body uses the operator on plain values.

Set relations (`⊆`, `∈`), the repetition operators and a block's `proto` of
an operator keep the registry path.
