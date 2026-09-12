# bench-threads gets a single-threaded twin, and indexed assignment gets a benchmark at all

`bench-threads` was one number mixing two independent things: how expensive a
container operation is, and how well that operation scales across cores. When
the row moved, nothing in `bench-history.tsv` could say which one moved.

That is not hypothetical. The row had sat at 1.5-2.0x rakudo — the only losing
series in the suite — and the obvious reading, that the concurrency work had
regressed, was wrong. Decomposing it ([#8069](https://github.com/tokuhirom/mutsu/issues/8069))
found:

- spawning four workers: **0.04x** rakudo, i.e. 25x faster;
- pure compute in four workers: scales **3.8x** on four cores and beats rakudo
  outright;
- a single `@a[$i] = $v`, **single-threaded, with no `start` in the program at
  all**: ~1850 ns against rakudo's ~40 ns.

The threading machinery is in good shape. The row was dominated by a defect with
nothing to do with threads, and no series in the history could have said so.

## bench-threads-serial

`benchmarks/bench-threads-serial.raku` runs the same total work as
`bench-threads.raku` — 600000 celled reads and 80000 celled element stores,
through the same `:=`-bound containers — on the mainline with no `start`
anywhere. The two rows then answer separate questions:

| what moved | what to read |
| --- | --- |
| a container read/write got more expensive | `bench-threads-serial` |
| concurrency got worse | `bench-threads / bench-threads-serial` |

Two details are load-bearing. The `:=` bindings stay: a plain `my @a` is served
by the name-keyed shared-variable lanes and would exercise neither the
`ContainerRef` cell nor ADR-0068's read-side guard, so the twin would no longer
measure the same storage path (the point `bench-threads.raku`'s own header and
[#7613](https://github.com/tokuhirom/mutsu/issues/7613) make). And the read
section is four 150000 loops rather than one flat 600000 loop, so the twin's
checksum is **identical** to `bench-threads`'s — 150000 is not a multiple of
256, so a flat loop would sum a different set of elements and the two files
could drift apart unnoticed.

At the time of writing: `bench-threads-serial` 1.94x rakudo, `bench-threads`
2.04x, both against v2026.07.

## bench-index-store

The second blind spot was plainer: **nothing in `benchmarks/` wrote through a
subscript at all.** `bench-array` does push/map/grep/sort/reverse, `array-ops`
does grep/map/elems, `hash-access` and `bench-hash` read. `@a[$i] = $v` and
`%h{$k} = $v` are among the most common statements in the language and had no
coverage, which is the direct reason a ~46x gap on the array form stayed
invisible until someone profiled a threaded benchmark for an unrelated reason.

`benchmarks/bench-index-store.raku` covers the array form, the hash form, and
the chained form (`@a[$i][$j] = v`, which takes a different store funnel —
ADR-0068 §4 step 3). It deliberately uses the simplest shape the store path has:
a plain non-negative `Int`/`Str` key, a plain rvalue, no constraint, no default,
no shape, no `:=`. That is the shape the fast path is supposed to serve, so any
cost it shows is cost the fast path is failing to avoid. Today: 2.39x rakudo.

Both files produce the same checksum under mutsu and under rakudo, so a wrong
answer fails them as loudly as a slow one.
