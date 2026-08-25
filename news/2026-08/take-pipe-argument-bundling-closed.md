# `take |(...)` bundles its arguments instead of over-flattening

`take |($a, $b)` passes two positional arguments to `take`, which bundles them
into a single gathered `List` item; only an explicit `Slip` flattens. mutsu used
to treat the pipe as a Slip, so

```raku
my \l = gather for 1..10 -> $a, $b { take |($a, $b) }; say l.raku;
```

yielded `(1, 2, ... 10).Seq` instead of `((1, 2), (3, 4), (5, 6), (7, 8), (9, 10)).Seq`.

The ticket filed for this was stale — `956c879f3` ("fix: bundle piped take
arguments") had already fixed it in `src/compiler/stmt.rs`, pinned by
`t/take-pipe-slip-over-flattens.t`, which covers the piped form, the explicit
`.Slip` form that must still flatten, and the looped case. Re-running all three
of the ticket's repros verbatim against current `main` gives identical output
under `raku` and `mutsu`, including the minimal isolation `gather { take |(1,2) }`
→ `((1, 2),).Seq`. The ticket was never removed when the fix landed; this closes
the bookkeeping.
