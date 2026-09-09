# `subst` and `trans` declare the named arguments they read

ADR-0070's signature survey reports only a lower bound for methods whose
adverbs arrive through a slurpy. The current Rakudo measurement is:

```
subst          -- (+*%options)
trans          -- (+*%_)
```

This slice records the manually established sets in
`src/builtins/accepted_nameds.rs`. `subst` keeps its global, match-count,
position, continuation, transform, and match-style controls, including their
short and long spellings. `trans` keeps `:s`/`:squash`, `:d`/`:delete`, and
`:c`/`:complement`. The extra match-control names are preserved at the
dispatch boundary even where mutsu does not yet consume them.

The focused regression test covers working `subst` and `trans` adverbs and an
unknown named. This is hardening: neither method changed its `:qqzz9` sweep
answer before the row was added. The campaign's constructor and unrelated
plain-call residue remain open.
