# `roast/S17-supply/syntax.t` takes ~57s wall and ~610 CPU-seconds

The single file `roast/S17-supply/syntax.t` (90 tests) runs for **57 seconds
wall clock** on a release build, consuming **~610 CPU-seconds** — roughly 11
cores' worth of work for one supply-syntax test file. `raku` runs it in well
under a second.

```
$ MUTSU_FUDGE=1 prove -e 'target/release/mutsu' roast/S17-supply/syntax.t
All tests successful.
Files=1, Tests=90, 57 wallclock secs ( 0.02 usr 0.01 sys + 554.92 cusr 55.98 csys = 610.93 CPU)
```

The CPU/wall ratio says the time is spent spinning, not computing: something in
the supply/react runtime is busy-waiting rather than blocking on a condvar.

## Why it matters

It is the whole margin of the roast budget. CI run 31191459333's `jit-stress`
job failed with

```
roast/S17-supply/syntax.t   Dubious, test returned 124 (wstat 31744)
  Parse errors: Bad plan.  You planned 90 tests but ran 70.
```

— the timeout shape, on a file that needs 57 of its allotted seconds even on an
idle machine. Under the parallel load of a full roast sweep it does not fit.
This will keep producing "flaky" CI reds until the spin is fixed; quarantining
it in `flaky-tests.txt` would only hide a real performance defect.

## Not a regression

Measured on `75b0ad4ca` (before the supply emitter-stamp work of #6044/#6047):
identical 57s / 610 CPU-s. The slowness predates that campaign.

## Where to look

`perf record` the file and look for the busy loop. Likely candidates are the
react drive loop's polling (`vm/vm_react_loop.rs`, `vm_react_subscriptions.rs`)
and `Self::sleep_for_supply_delay` / the promise-wait paths, any of which
spinning would explain a ~11:1 CPU-to-wall ratio.
