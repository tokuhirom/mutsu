# Pulling a deferred map now pulls the deferred maps it produced

[ADR-0058](../../docs/adr/0058-map-grep-produce-a-deferred-seq.md) made `.map`
return a `Seq` whose `SeqSource::MapGrep` body runs the callback at first
consumption, and §8.2 lists the pure-value readers that had to learn to force
one first (`reify_map_grep_seq`). One case was missed: a callback that itself
returns a deferred Seq.

```raku
say [1].map(-> $e { [2].map(-> $x { "STOP" }) }).raku;
```

Pulling the outer body ran the outer callback, which produced an **unpulled**
inner `MapGrep` body — and the same pure readers then saw ADR-0034's empty seed
one level down:

| read | rakudo | mutsu (before) |
|---|---|---|
| `.raku` | `(("STOP",).Seq,).Seq` | `(().Seq,).Seq` |
| `.gist` / `say` | `((STOP))` | `(())` |
| `.Str` | `STOP` | (empty) |
| `.flat.join` | `STOP` | (empty) |

The fix is at the single chokepoint rather than at each reader: the `MapGrep`
arm of `pull_seq_source` (`src/vm/vm_helpers_lazy.rs`) now reifies any deferred
Seq among the elements its own pull just produced, recursively. Depth is bounded
by how deeply the callbacks nest, and every level is finite for the same reason
the top level is — a `MapGrep`'s `items` were materialized at the `.map` call.

Laziness is unchanged: the outer callback still runs at first consumption
(`t/map-callback-runs-at-consumption.t` and `t/try-sink-semantics.t` are
untouched and green). Only the elements a pull has *already* produced are
descended into.

## Found while attempting ADR-0058 step 3, which is now parked

Step 3 (make the listop `map &f, @xs` form defer like the method form) is a
five-line diff and was green on `make test`, but the mandatory full `make roast`
aborted `roast/integration/99problems-21-to-30.t` with a stack overflow. That
turned out to be a **step 2 hole that step 3 only routes more programs onto**,
and it reproduces on `main` today with the method form:

```raku
sub g(@sizes) {
    return "STOP" if @sizes == 0;
    [1].map(-> $e { g(@sizes[1..*]).map(-> $x { $x }) })
}
say g((2,1)).raku;
```

rakudo terminates at depth 3 with `@sizes` empty; mutsu recurses forever,
reading `@sizes` as depth 1's `(2, 1)`. A deferred `MapGrep` carries `items`,
`func` and `fatal` but **no frame**, so the callback runs under whatever env is
active at the pull — while the pre-ADR-0058 `create_lazy_map_list`, which
snapshots `self.env` at the `.map` call, gets the same program right. The two
deferral mechanisms step 4 wants to collapse therefore disagree about which
lexical frame a deferred callback belongs to.

Recorded with its options and its measurements in
`todo/deep/deferred-map-callback-runs-in-the-consuming-frames-env.md` and
ADR-0058 §9.1; grep's separate blocker (its matched-slot `ContainerRef`
promotion and identity-keyed writeback) is measured in §9.2.

## Gates

`t/nested-deferred-map-seq-is-pulled.t` (8 rows, verified green under `raku`
too; the listop row is `todo`-marked until step 3 lands).
`t/map-callback-runs-at-consumption.t` and `t/try-sink-semantics.t` unchanged
and green. `make test` PASS. Full local `make roast` PASS, as ADR-0058 §5
requires for any change in this family.
