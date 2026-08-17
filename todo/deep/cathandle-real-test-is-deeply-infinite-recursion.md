# Real Test.rakumod's `is-deeply` infinitely recurses on an `IO::CatHandle`-backed lazy Seq

Originally filed as `todo/tickets/cathandle-handles-wrongly-lazy-array.md`
("`IO::CatHandle.handles` is wrongly lazy, and wrongly an Array"). Re-investigated
2026-08-18 while working the `todo/tickets/` backlog oldest-first: **that
ticket's headline claim is now stale** — verified fixed, see below — but a
much more serious bug surfaced underneath it, which is why this file replaces
it in `todo/deep/` rather than simply closing it out.

## The original claim is stale

```raku
my $cat = IO::CatHandle.new: "/etc/hostname";
say $cat.handles.^name;    # now: Seq  (ticket claimed: Array)
say $cat.handles.is-lazy;  # now: False (ticket claimed: True)
```

`t/io-cathandle-lazy.t` passes all 13 subtests under the default (native)
`Test` module, including "handles reports Seq" / "handles is not lazy
externally" / "CR-LF is a single line ending" / "lazy .handles: reads 2 lines
per handle" — the four assertions the original ticket's table and repro
described as broken. Nothing in `git log` on `io_cathandle.rs`/`value_lazy.rs`
between the ticket's filing (2026-08-13) and now looks like a targeted fix, so
this was likely already fixed indirectly by unrelated `LazyList`/`Seq`
introspection work (`methods_introspect.rs`'s `ValueView::LazyList(_) =>
"Seq"` arm, gated on `!in_array_context()`/`!in_list_context()`, already
covers this correctly and is unconditional — not mode-specific — so it was
probably never actually broken the way the ticket described, or was fixed as
a side effect of `news/2026-08/lazy-seq-cache-list-name.md`).

## The real, deeper bug: infinite recursion under `MUTSU_REAL_TEST=1`

The ticket's own "under `MUTSU_REAL_TEST=1`" caveat is where the real problem
lives, and it is much worse than a comparison mismatch:

```raku
use Test;
plan 1;
my $seq = 0;
sub tmpfile($content) {
    my $p = $*TMPDIR.add("mutsu-cat-{$*PID}-{$seq++}");
    $p.spurt: $content;
    $p
}
my $cat = IO::CatHandle.new: tmpfile("a\r\nb\r\nc");
my $lines = $cat.lines;
is-deeply $lines, ("a", "b", "c"), 'CR-LF is a single line ending';
```

Run with `MUTSU_REAL_TEST=1 target/debug/mutsu <file>` (the experimental flag
that switches to the vendored `modules/Rakudo-Core/lib/Test.rakumod`, per
`todo/deep/vendor-real-test-module.md` — mutsu's *default* `Test` provider is
still the native `runtime/test_functions.rs`, so **nothing whitelisted
depends on this today**):

- With the default 8 MiB stack (`ulimit -s` = 8192): reproduces a **stack
  overflow** (`thread '<unknown>' (...) has overflowed its stack`) after
  ~30 CPU-seconds, 3/3 standalone runs.
- With a 64 MiB stack (`ulimit -s 65536`): no crash, but the process just
  **hangs** (times out at 30s, still climbing) instead of completing —
  proving this is genuinely **unbounded recursion**, not merely "deep but
  finite." A bigger stack does not fix it; it just delays the crash.
- Oddly, one run launched under `rust-gdb -batch -ex run` completed normally
  and printed `ok 1` within ~8 seconds — not yet explained. Either a
  scheduling/timing-dependent branch in the recursion's termination check, or
  gdb's ptrace somehow changes which code path executes. Whoever picks this
  up should not assume the gdb-clean run means the bug is flaky/rare — the
  plain-binary reproduction is the reliable one (3/3), and it is a genuine
  hang/crash, not a benign slow path.

### Narrowing already done (to save the next session's time)

None of these crash or hang — the problem is specific to `is-deeply` (real
Test.rakumod's own comparison implementation, not the `eqv` operator) applied
to an `IO::CatHandle`-backed lazy Seq specifically:

- `say $lines.list;` (forcing the lazy list to a plain List) — fine, prints
  `(a b c)`.
- `say $lines.gist;` / `say $lines.raku;` — fine, prints `(a b c)` /
  `("a", "b", "c").Seq`.
- `say $lines eqv ("a", "b", "c");` — fine (returns `False` quickly, both
  with and without `MUTSU_REAL_TEST=1` — a separate, smaller correctness
  question: Rakudo's `eqv` on two structurally-equal-content Seqs of
  different origin should likely also say `True`, not investigated here).
- `is-deeply @a.Seq, (1, 2, 3), ...` (a plain array-backed `.Seq`, not
  CatHandle-backed) under `MUTSU_REAL_TEST=1` — fine, `ok 1`.

So the trigger needs BOTH: (a) the real `Test.rakumod`'s own `is-deeply`
implementation (not the native provider, not bare `eqv`, not `.gist`/`.raku`
rendering), AND (b) a Seq specifically backed by `LazyList::new_cat_pull`
(`src/value/value_lazy.rs:452`, consumed via `CatPullMode::Lines`/`Handles` in
`src/vm/vm_helpers_lazy_pull.rs`) — a plain array-backed Seq does not trigger
it.

## Where to look next

- Read `modules/Rakudo-Core/lib/Test.rakumod`'s `is-deeply` (and whatever
  internal structural-equality helper it calls — Rakudo's real one is not a
  simple `eqv`, it does its own recursive walk for diagnostics) to find what
  it does differently from bare `eqv` that could recurse on a CatHandle-backed
  LazyList specifically.
- The CatHandle's own attribute cell (`sources`, the active handle, an
  `on-switch` callable, `path`) is a plausible source of an accidental cycle
  if `is-deeply`'s walk touches instance attributes reflectively (e.g. via
  `.^attributes` introspection) rather than only iterating Seq elements — a
  `LazyList::new_cat_pull` element pull re-reads the SAME live `cat` value
  (`cat.clone()`, sharing its attribute cell) on every pull, so if the
  comparison walk ever recurses into "the value this Seq is built from" (as
  opposed to only its yielded elements) it could loop back into the same
  CatHandle indefinitely.
- Confirm whether `rust-gdb`'s clean run is reproducible (run it 3-5 more
  times) before trusting it as a real alternate outcome versus a fluke of
  that one session (a stale debug build, a leftover tmpfile from a prior run
  changing `$seq`'s effective path, etc.).
- Bisect which part of `is-deeply`'s Rakudo implementation is responsible by
  trimming `Test.rakumod` (see `todo/tickets/*` files elsewhere in this repo
  for the brace-balanced-chunk bisection technique already used successfully
  on this exact file — do NOT truncate by line number, `Test.rakumod` calls
  `_init_vars()` near the top from a routine declared far below).

## Why this is `todo/deep/`, not a ticket

- Reproduction is CPU-heavy (~30s to crash) and was observed to disagree
  between a plain run and a gdb-attached run — needs a session with room to
  characterize the non-determinism, not a quick fix.
- The likely root cause (a reflective/attribute-walking structural comparator
  looping back into a live, shared, self-referential attribute cell) is a
  genuine correctness question about how Rakudo's real `is-deeply` interacts
  with mutsu's `Gc`-shared instance attributes under a lazy pull, not a
  narrow, self-contained fix.
- It is gated behind the experimental `MUTSU_REAL_TEST=1` flag
  (`todo/deep/vendor-real-test-module.md`'s campaign) — nothing whitelisted
  depends on it today, so it does not block current roast progress, but it
  will need solving before that vendoring campaign can flip the default.

## Repro files used during this investigation (not committed; recreate as needed)

```raku
use Test;
plan 1;
my $seq = 0;
sub tmpfile($content) {
    my $p = $*TMPDIR.add("mutsu-cat-{$*PID}-{$seq++}");
    $p.spurt: $content;
    $p
}
my $cat = IO::CatHandle.new: tmpfile("a\r\nb\r\nc");
my $lines = $cat.lines;
is-deeply $lines, ("a", "b", "c"), 'CR-LF is a single line ending';
```

Run: `MUTSU_REAL_TEST=1 timeout 45 target/debug/mutsu <file>` (default 8 MiB
stack reproduces the crash in ~30s; `ulimit -s 65536` first to see the
underlying hang instead).
