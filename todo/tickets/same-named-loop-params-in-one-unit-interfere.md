# Two `for` loops in one unit that name their parameters the same interfere

Two independent `for` loops, in separate blocks, that happen to use the same
parameter names do not behave independently: an `is rw` loop earlier in the unit
changes what a *later, unrelated, non-rw* loop's closures capture.

```raku
{
    my @b = 1, 2, 3, 4;
    my $q;
    for @b -> $x is rw, $y is rw { $q = -> { $x } if $x == 1 }
    @b[0] = 99;
    say $q();        # raku 99, mutsu 99
}
{
    my @a = 10, 20, 30, 40;
    my @c;
    for @a -> $x, $y { @c.push(-> { $x }) }
    say @c>>.();     # raku [10 30], mutsu [30 30]   <-- per-iteration identity lost
}
```

Delete the first block, or rename its parameters, and the second prints
`[10 30]`. It is the second block that is wrong, and nothing in it changed.

## Scope

- Pre-existing: reproduces on `main` (verified at `a1d291bfa`, before
  `news/2026-09/multi-param-rw-closure-reads-through-the-element.md` landed).
- The interference runs both ways. The single-statement form of the *first*
  block on its own (`my @a=1,2,3,4; my $c; for @a -> $x is rw, $y is rw { $c =
  -> { $x } if $x==1 }; @a[0]=99; say $c()`) used to print `1`; in the two-block
  file above it printed `99` even before the fix — the later loop's presence was
  changing the earlier loop's answer.
- Same shape as the landmine
  `news/2026-09/proxy-fetches-at-the-container-store.md` closed for `Proxy`
  elements, and as the one
  `todo/tickets/closure-for-loop-param-hijacked-by-same-named-captured-outer.md`
  describes: a per-*compilation-unit*, name-keyed mechanism standing in for a
  per-*binding* one.

## Likely cause

Same-named `my` locals share one local slot per compiled chunk, and several
closure-capture mechanisms are keyed by NAME over the whole `CompiledCode`
(`captured_mutated_locals`, `needs_cell_locals`, `for_loop_param_syms`,
`free_var_writes`). One loop's `$x` being captured-and-mutated therefore marks
*the name* for the whole unit, so the other loop's `$x` takes the boxed/cell
path (or the frozen path) it should not.

This was found while fixing the read half of ADR-0045 rows 11/20: the runtime
`active_loop_rw_param_names` stack that fix introduced is deliberately
runtime-scoped rather than a per-`CompiledCode` name set for exactly this
reason (a compile-time set let one loop's `is rw` exempt another loop's non-rw
parameter). The remaining interference is in the older, compile-time sets.

## Why it is a ticket rather than a fix

Making these sets per-binding rather than per-name means giving same-named
locals in disjoint scopes distinct slots — the `MUTSU_SHADOW_SLOTS` campaign
(`docs/lexical-scope-slot-campaign.md` §1.3/§1.4), which is gated off by
default. Before that lands, any narrower fix is another name-keyed heuristic.
Check whether turning the shadow-slot gate on makes the repro above pass; if it
does, this ticket is really a datapoint for that campaign rather than its own
piece of work.

## Reproduce

The file above, no fixtures. `t/for-loop-element-alias.t` names two of its
non-rw rows `$m`/`$n`/`$w` to steer around this; that is the marker to remove
when this is fixed.
