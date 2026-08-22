# The residual try-cell divergences are eager `.map`, not a mis-placed `LazyList` force

`todo/deep/residual-try-cell-eager-seq-reification-divergences.md` carried a
twelve-cell matrix where mutsu is more forgiving than raku: a `die` or `fail`
inside a `map` callback under a statement-position `try` is caught by that
`try` in mutsu and escapes it in rakudo. The ticket — and
[ADR-0034](../../docs/adr/0034-seq-reification-is-in-place-and-distinct-from-consumption.md)
§6 quoting it — attributed this to mutsu forcing a `map`-produced `LazyList` at
the assignment/call boundary, and pointed at `force_lazy_list_vm`'s callers.

Re-measuring all twelve cells against a current `main` build (none had been
fixed by an intervening change) showed that diagnosis is wrong. `(1..3)` is a
**finite** range, so `is_lazy_pipe_source` is false and no `LazyList` is ever
built: `dispatch_map_method` materializes the source and runs the callback
**immediately**, inside the `try`. There is no deferred value whose force could
be moved. mutsu's `try`/sink placement is not at fault either — it was already
verified rakudo-conformant in
`news/2026-08/try-statement-sink-semantics-pinned.md`.

The actual root cause is that `.map` and `.grep` are eager over a finite source
in mutsu and lazy in rakudo, which is observable with no `try` and no exception
at all:

```raku
my $s = (1..3).map({ say "side $_"; $_ }); say "before"; say $s.List;
# raku:  before / side 1 / side 2 / side 3 / (1 2 3)
# mutsu: side 1 / side 2 / side 3 / before / (1 2 3)
```

mutsu already *knows* some callbacks must be deferred — `dispatch_map_method`
routes a callback whose body contains `return`, or is a `...` stub, through
`create_lazy_map_list` — but "the body contains a `die`" is not a predicate
worth writing, since the throw can be indirect. That enumeration is the band-aid
the divergence grows out of.

The design is recorded as
[ADR-0058](../../docs/adr/0058-map-grep-produce-a-deferred-seq.md) (`Proposed`):
give ADR-0034's `SeqSource` a `MapGrep` variant so a `.map` result is a `Seq`
whose body is not yet reified, and let the reify/consume split that ADR already
built do the deferring — rather than routing map through `LazyList` (a per-call
`Env` clone and a fourth representation of a lazy sequence) or widening the
syntactic predicate.

Four of the twelve cells turned out to be a **different, narrower** bug, split
out in ADR-0058 §1.4: the `...`-stub cells already defer, and already match raku
exactly once the enclosing `try` is removed. What diverges there is how a `fail`
raised *during the force* resolves when a `try` sits lexically between it and
the routine — mutsu returns it from the routine as a `Failure`, rakudo throws.

Everything is now pinned by `t/map-callback-runs-at-consumption.t`: 23 rows,
verified 23/23 under real `raku`, of which mutsu passes 12 outright (real
already-correct behaviour that the fix must not regress) and carries 11 as
`todo`. Un-`todo`ing the nine ADR-0058 rows is that ADR's completion signal.
