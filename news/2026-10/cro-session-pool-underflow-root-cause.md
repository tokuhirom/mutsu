# The Cro::HTTP pool-thread `Gc::drop` underflow was the DESTROY-snapshot bug

[#11334](https://github.com/tokuhirom/mutsu/issues/11334) reported an
intermittent `Gc::drop strong-count underflow` on a `Gc<PairData>`, dropped on
a pool thread while running Cro::HTTP's `t/http-session-inmemory.rakutest`.
It had stayed open as a suspected cross-thread refcount race.

It was not a thread race. With the fix from
[#12319](https://github.com/tokuhirom/mutsu/pull/12319) disabled, the file
fails on every run under `MUTSU_GC=on MUTSU_GC_EVERY_CANDIDATE=1024`, with the
same backtrace (a pool thread dropping a `Gc<PairData>`) and a failing session
subtest. The cause is the one #12319 fixed: a DESTROY snapshot taken while a
garbage cycle was reclaimed kept a Capture box alive, together with Pair handles
whose counts trial deletion had already deducted. With current `main`, the file
passes 50 runs in a row under the same environment.

`t/vm/destroy-snapshot-gc-counts.t` adds a Raku-level pin next to #12319's
unit test: a child process builds DESTROY-bearing cycles that hold live Pairs
in Capture attributes, with a collect armed every 64 candidates. Without the fix,
a debug build reports the underflow.
