# roast `6.c/APPENDICES/A03-older-specs/01-misc.t` is whitelisted

The 6.c appendix of older-spec features passes 9/9, as on rakudo once the
file is fudged. It went from aborting at its `subst-mutate` group to a clean
run through seven fixes, each with its own news entry:

- `Cool.subst-mutate` (#18813);
- a Callable `:nth` (#18822);
- `:nth(Seq)` and the empty multi-`:nth` match (#18829);
- mutating methods on an `($x = EXPR)` receiver (#18843);
- a stable identity for a container-holding Pair across `.freeze` (#18865);
- the `use v6.x` first-statement rule and `use v6c` (#18872);
- `$CALLER::_` writes (#18882).

Its row leaves `TODO_roast/BLOCKERS.md`.
