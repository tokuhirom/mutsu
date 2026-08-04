# Named parameters narrow by one boolean step, then declaration order decides

`todo/tickets/multi-tie-break-declaration-order.md` (opened when
`news/2026-08/named-params-do-not-narrow.md` removed named *types* from the
narrowness comparison) asked for a real ordering key so that equally-narrow
multi candidates resolve the way rakudo resolves them: the one declared first
wins. mutsu had none — `sort_candidates_by_specificity` broke an equal-rank tie
on the **registry key string**, and the candidates themselves were collected by
iterating a hash map, so nothing upstream preserved declaration order either.

Two things were wrong, and the second is why the first was never visible.

## The ordering key

`FunctionDef::decl_order` already existed, but only `insert_token_def` stamped
it (for the Longest-Token-Match tie-break between proto `token` candidates);
every other registration path left it 0. It is now stamped from a single
`runtime::resolution::next_decl_order()` counter at all nine `FunctionDef`
construction sites — plain subs, `our`/`GLOBAL` subs, protos, class and role
methods — and used as the final key in both `sort_candidates_by_specificity`
and `choose_best_matching_candidate`'s ranking sort. The counter is global
rather than per-`(package, name)`: only the relative order *within* one
candidate set matters, and every candidate of a set is registered by the same
top-to-bottom pass over its unit. The ticket worried that precompilation would
lose the stamp, but the precomp cache stores the parsed AST (`Vec<Stmt>`), not
`FunctionDef`s — the stamp is always applied at runtime registration.

## What named parameters actually contribute to narrowness

With the ordering key in place the ties still resolved wrongly, because two
other inputs let named parameters move the ranking:

- `candidate_specificity_rank_for_args` ranked candidates by **how many** named
  parameters they declare.
- `candidate_type_distance` skipped a *typed* named parameter but charged an
  **untyped** one the flat `1000` it charges an unbindable parameter — so a
  candidate lost the secondary key purely for declaring one named more.

Probing rakudo with `&f.cando(\(...))` shows the real rule is a single boolean,
not a count:

    multi p1()     { }   #  cando(\()) reports (:$x) first, in either
    multi p1(:$x)  { }   #  declaration order — declaring a named is narrower

    multi p2(:$x)      { }   #  cando(\()) reports them in DECLARATION order —
    multi p2(:$x, :$y) { }   #  declaring *more* nameds is not narrower

So the count is gone from the rank tuple and named parameters no longer move
the type distance at all; `candidate_declares_named` replaces both as one
boolean step, placed by what rakudo does:

- BELOW positional narrowness and type distance — `multi q(Int $a)` beats
  `multi q(Cool $a, :$x)` for `q(1)`, in either declaration order;
- ABOVE the optional-positional count — `multi r($a?, :$x)` beats
  `multi r($a)` for `r(1)`;
- and a slurpy still loses to a plain positional on distance, so
  `multi r3($a)` beats `multi r3(*@a, :$x)`.

## What it fixes

The `Digest::SHA3` blocker from `todo/tickets/digest-dist-blockers.md` §6:
`Keccak`'s two candidates differ only in the extra `:$outputByteLen` named, so
mutsu ran the wide one for a call that omitted it — reporting an
uninitialized-value warning and, in the real module, recursing into the
candidate that was supposed to delegate to its sibling. Calls now reach the
right candidate. (`Digest::SHA3` still stops at a separate bug: `samewith`
inside a lazy `gather` loses its dispatch frame.)

Pinned by `t/multi-named-tie-declaration-order.t`, whose 17 expectations were
all taken from rakudo.
