# gather takes are lost after a prior Callable-in/Array-out csv call (90_csv 495 + end abort)

Found 2026-08-12 while closing out the 90_csv.t frontier (after the two
`s///` fixes recorded in news/2026-08/subst-topic-mirror-and-replacement-precedence.md).
This is the last real mutsu bug in 90_csv.t: it explains failing test 495
("data from CODE/AR" got `[]`) and the end-of-file abort ("Cannot shift from
an empty Array" — the next `csv (in => $fno, headers => "auto")` reads the
empty file test 494 wrote and dies shifting `@in`).

## Symptom

In Text::CSV's `method CSV`, the Callable-in arm builds

    @in = gather while $in() -> $r { ... take $r ... };

In the failing call, the while loop runs to completion (the row-provider sub
gets called all 4 times — its `$idx` ends at 4) but `@in` reifies EMPTY: the
takes vanish. A writer-loop probe shows `in-elems=0` while a probe after the
csv call shows `idx=4`. So it is not the provider, not the loop, and not
rrange filtering — the take values are dropped between `take` and the gather
collector.

## Trigger (state pollution from an EARLIER csv call)

Bisecting t/90_csv.t (strict oracle: tests before the victim pass, the victim
fails with `got: []`): the poison is the "Pre-existing AOH" block —
specifically `csv (in => &provider, out => @prefill)` where `@prefill` is an
`Array:D` holding one Hash record (out=Array:D + `$out[0] ~~ Hash` forces
`headers = "auto"`, and the header row is consumed via `@in.shift`). That call
itself SUCCEEDS; it poisons some interpreter-global gather/take state that a
LATER `gather while <callable>() -> $r { take $r }` inside csv then falls
victim to.

Not yet minimal below the module level:

- A pure-mutsu imitation (gather-over-sub + shift + push loop, then a second
  gather) does NOT reproduce.
- A two-call Text::CSV script (poison csv + victim csv, no Test module, no
  extra fixtures) does NOT reproduce either — the verified repro still
  carries part of the test-file header (Test.pm, the `_90in.csv` fixture with
  open `$io-in`/`$io-out` handles, `s-in`, `sleep-time`, `inok`); each of
  those paragraphs resisted paragraph-level removal under a strict oracle.
- **Reduction trap**: a line-level greedy pass with a loose oracle ("some
  test says `got: []`") happily deleted the `@data`/`$full-aoa` declarations
  and produced a degenerate 'repro' that fails for undefined-variable
  reasons. Any further reduction must assert the poison test PASSES, the
  write test PASSES, and the victim fails with `got: []` against the real
  expected list.

## Repro (module-level, deterministic, verified)

`tmp/gather-repro.t` (73 lines, 3 tests) against the Text::CSV clone
(`tmp/text-csv`, github.com/Tux/CSV):

    cd tmp/text-csv && target/debug/mutsu -I lib ../gather-repro.t
    # ok 1 (poison), ok 2 (write), not ok 3 - data from CODE/AR: got []

or run the full suite: `prove -e "mutsu -I lib" t/90_csv.t` — fails 495 and
aborts after 496. (Test 159 is NOT a mutsu bug: rakudo 2026.06 +
Slang::Tuxic 0.0.5 fails it identically; verified locally.)

## Where to look

- `take` routing: `exec_take_op` → `take_value` appends to the top of
  `Interpreter::gather_items` (a `Vec<Vec<Value>>` stack). The two reify
  paths — eager force (`vm_helpers_lazy.rs:~324`) and lazy pull
  (`vm_helpers_lazy_pull.rs:~142`) — each push a collector + take-limit and
  pop back to a saved depth. A stack imbalance left by the poison call (e.g.
  a collector pushed by an abandoned/partially-consumed lazy pull that never
  popped, or a mismatch between `gather_items` and `gather_take_limits`)
  would make later takes land in (or be limit-checked against) a stale entry.
- The poison call's distinctive shape: the gather Seq assigned to `@in`, then
  `@in.shift` (headers auto) and a `for @in` writer loop pushing to the
  caller-supplied `Array:D` — i.e. multiple partial consumers of the same
  lazy gather within nested method frames.
- `lazy_pull_entry_call_depth`, `gather_suspend_pending`,
  `gather_for_loop_resume` are adjacent per-pull state that is saved/reset;
  check whether every error/early-return path restores them.

## Impact

Blocks 90_csv.t (494/496 otherwise, with 159 raku-parity). Any program where
a gather runs after a similar partially-consumed gather is at risk of
silently losing takes — worth fixing on general grounds, not just for CSV.
