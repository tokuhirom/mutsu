# A `when`/`default`-tail block now evaluates correctly when nothing matches

```raku
my @a = (1, "a", 2).map({ when Int { "int" } });
say @a.raku;
# raku:  ["int", 0, "int"]
# mutsu (before): ["int", "a", "int"]

my @b = (1, "a", 2, "b").grep({ when Int { True } });
say @b.join(",");
# raku:  1,2
# mutsu (before): 1,a,2,b (nothing filtered)
```

Rakudo's rule: a non-matching `when` statement does not "produce nothing" —
it evaluates to the falsy result of its own condition test, and control
falls through to the next statement. If a `when`/`default` chain is a
block's final statement and no branch matches, that falsy test result IS the
block's value: `Bool::False` for a value matcher (`when 2`), or `Int 0` for a
type-object matcher (`when Int` — Rakudo's `nqp::istype` boxing artifact,
observable and stable across classes/roles/subsets/smileys/`constant`
aliases).

mutsu's inline `.map`/`.grep`/`.first` fast paths (`eval_map_over_items`,
`try_first_match_batched`, `eval_map_over_items_rw`,
`eval_grep_over_items_with_mutated`) computed a non-matching block's value as
`vm.last_stack_value().or_else(|| vm.env().get("_"))` — when nothing
matched, nothing was pushed to the stack, so this fell back to the CURRENT
TOPIC (the original, unfiltered item) instead of the falsy test result. This
made `.grep` with a when-only block filter nothing at all (every item's
"value" was just itself, always truthy) and made `.map` leave non-matching
items unchanged.

Fixed with a compile-time gate plus a runtime marker: `exec_when_op` now
records the failed test's falsy value (`Interpreter::when_nonmatch_value`)
on every non-match, and the four fast-path sites consume it — but ONLY when
the block's tail statement is itself a bare `when`/`default` chain (a
compile-time check, `tail_is_when_chain`), so the topic fallback keeps
firing unchanged for every other tail shape it's genuinely needed for (the
`<->`/`.=map` rw writeback machinery, reflecting a possibly-mutated `$_`).

The same wrong-value gap still exists outside map/grep/first (a direct block
call, `do { when ... }`, a bare `given`/`when`) — filed as
`todo/tickets/when-nonmatch-value-outside-map-grep.md`, since fixing it
generally requires a stack-hygiene sweep across statement-sequence
compilation, not a point fix.

Pin: `t/when-only-block-nonmatch-value.t` (11 assertions, including a `todo`
marker for the follow-up ticket's scope).
