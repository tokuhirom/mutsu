# Fix an 8.5x instruction-count regression in `.sort`'s inline comparator fast path

`benchmarks/bench-array.raku`'s deterministic instruction count
(`scripts/bench-det.sh`) jumped from ~246M to ~2.36B `Ir` (+855%) at
`0ab4cfeee` ("fix: dispatch user numeric coercion in sort", #8586). Bisected
with `valgrind --tool=callgrind` across isolated `git worktree` checkouts of
the commit and its parent, confirming the regression reproduces on
`benchmarks/bench-array.raku` itself (246,104,260 -> 2,355,679,192 Ir,
byte-for-byte reproducible across repeated runs) but NOT on a reduced
push-then-sort-only repro, which pointed at an interaction with the
benchmark's earlier `.map`/`.grep` calls rather than sort in isolation.

Root cause: `#8586`'s fix added `can_inline_numeric_cmp`, a guard that skips
the merge sort's inline `{ $^a <=> $^b }` fast path whenever any element in
the array is a `ContainerRef` (or an `Instance`), because a container might
hold a user object whose `<=>` needs real method dispatch. But a `ContainerRef`
does not imply a user object — it is also what an ordinary `.grep`/`.map`
leaves behind: iterating a block topicalizes `$_` as an alias to the source
array's slot, which promotes that slot's element in place to a shared
`ContainerRef` cell (regardless of what value it holds). `bench-array.raku`
runs `@arr.grep(* %% 2)` right before `@arr.sort({ $^b <=> $^a })` on the
same array, so half of `@arr`'s 10,000 plain `Int` elements were
`ContainerRef`-wrapped by the time `.sort` ran, and `can_inline_numeric_cmp`
saw those and disabled the fast path for the *entire* sort — forcing full
`<=>` dispatch (a real sub call into the comparator block) for every one of
the ~130,000 comparisons a merge sort of 10,000 elements performs, instead of
one inline branch each.

Fix: `can_inline_numeric_cmp` now derefs through the container chain
(`Value::with_deref`, which also collapses nested `:=`-rebind cells) before
checking for an `Instance`, instead of treating the outer `ContainerRef`
shape itself as disqualifying. This is sound because the actual comparator
(`inline_numeric_cmp`) already falls back to `compare_values`, which derefs
`ContainerRef` itself, for any value shape its own fast arms don't match — so
a plain scalar sitting in a container was never the problem `#8586` needed to
guard against; only a container that resolves (after fully collapsing any
cell chain) to a user `Instance` still needs the real dispatch path, and
still gets it.

Verified with `valgrind --tool=callgrind` on `benchmarks/bench-array.raku`:
2,355,679,192 Ir (regressed) -> 250,089,277 Ir (fixed), against a
246,104,260 Ir pre-regression baseline — recovering the full regression to
within ~1.6%. All three builds (pre-regression, regressed, fixed) produce an
identical `checksum = 55002` from the benchmark, confirming this was a pure
performance regression with no behavior change.

Added `t/collections/transform/sort-inline-comparator-container-elements.t`,
which exercises the exact `.grep`-then-`.sort` shape that triggered the
regression (both for plain `Int` elements and for `#8586`'s own user-numeric
`Instance` case, so a future change can't silently reintroduce the slow path
for plain scalars, nor drop the real dispatch for user objects).
