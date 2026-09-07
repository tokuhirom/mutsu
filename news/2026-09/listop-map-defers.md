# The listop `map` defers too (ADR-0058 step 3)

`map &f, @xs` — the listop spelling — ran its callback at the call and answered
a `List`. `@xs.map(&f)` has answered a not-yet-run `Seq` since ADR-0058 step 2,
so the two spellings of the same operation disagreed about when the callback
runs, what type comes back, and whether an enclosing `try` can catch a `die`
raised inside it.

```raku
say (map { $_ * 2 }, 1..3).^name;                       # was List, now Seq
sub ee { my $s = map -> $x { print "RAN"; $x }, 1..3; print "T"; $s }
ee().List;                                              # was RANRANRANT, now TRANRANRAN
sub dying { my $s = try { map { die "boom" }, 1..2 }; "tail" }
```

Step 3 is a five-line diff — `builtin_map`'s non-rw tail returns
`Value::seq_deferred(SeqSource::MapGrep { .. })` instead of calling
`eval_map_over_items`. It was attempted and reverted on 2026-09-06 because the
full roast run aborted a whitelisted file with a stack overflow; that blocker
(a deferred callback reading its free variables from the consuming frame) was
closed first — `news/2026-09/deferred-map-callback-frame.md`.

## The interesting part: what the mandatory roast run found

ADR-0058 §5 makes a full local `make roast` mandatory for these steps, because
each one makes mutsu *stricter* in every consumer of a mapped sequence.
`make test` was green with step 3 in place — 3779 files, 39681 tests — and the
roast run still failed three files. None of the three was a step-3 special
case; all three were general defects that eager `map` had been hiding.

**1. A nested deferred map clobbered the enclosing one's capture.**
`eval_map_over_items` merges the callback's captured env into the running frame
and restores it afterwards, but the save set (`touched_keys`) listed only the
keys the merge *introduced*. Since the merge began overwriting a same-named key
(the free-variable rule that closed the previous bug), an overwritten key was
never restored — so an inner map left its own capture behind for the outer
map's **next** iteration:

```raku
sub inner(@sizes) {
    return $["END"] if @sizes == 0;
    map -> $e { map -> $g { "$e/$g" }, inner(@sizes[1..*]) }, ['a', 'b']
}
say inner((1,2)).map({ .List.raku }).join(' ; ');
```

| | |
|---|---|
| rakudo | `("a/a/END", "a/b/END") ; ("b/a/END", "b/b/END")` |
| before | `("a/a/END", "a/b/END") ; ("b/END",)` |

Iteration 1 was right and iteration 2 read `@sizes` as the recursive call's
`(2,)`. Every key the merge overwrites is saved and restored now.

**2. `categorize`'s mapper result was never forced.** Its keys are read through
`as_items`, a pure-value reader that cannot pull a deferred body (ADR-0034
§2.1), so `categorize({ map { … }, .comb }, …)` categorized *nothing* — an
empty hash. The one-line guard for exactly this, `reify_map_grep_seq`, was
already in the codebase, and the older deferral's `LazyList` force sits on the
adjacent line.

**3. `MAIN`'s return value was not sunk.** rakudo's `RUN-MAIN` sinks it, which
is why `sub MAIN() { map { print "ha" }, ^3 }` prints `hahaha`. mutsu dropped
the value, which was invisible while `map` was eager and printed nothing once
it was not. Both the fall-off-the-end and the explicit-`return` paths sink now.

## Result

`make test` PASS (3779 files / 39681 tests) and the full `make roast` PASS
(1436 files / 218962 tests). Pinned by `t/listop-map-defers.t` (10 rows) and
two new rows in `t/deferred-map-callback-frame.t`, all verified green under
rakudo as well; `t/nested-deferred-map-seq-is-pulled.t`'s listop row is
un-`todo`d.

ADR-0058 step 3b (`grep`, which needs the rw-cell promotion `map` has no
equivalent of) and step 4 (retiring `create_lazy_map_list`) remain open, and
§5's full `make roast` still gates them.
