# t/ tests get a per-file timeout multiplier

The nightly `jit-stress-tap` job timed out
`t/collections/lazy-seq/lazy-generator-scan-strict-force.t` with no test
reported (#11090). The file is not hanging and is not JIT-sensitive: its first
assertion strict-forces an endpoint-less closure sequence, which runs the
generator for the full 1,000,000-element bounded attempt before answering
`X::Cannot::Lazy` (the same cap the map/grep pipe force uses; Rakudo itself
never returns from such a force, and does complete a self-ending sequence of
300,000 elements, so the cap cannot shrink). That costs 1.6s on a release
binary but 25s on the opt-level-0 debug binary the stress jobs build, and
under `prove -j4` on a 4-core runner it overran their 90s per-file budget.

`scripts/run-t-test.sh` now has `per_file_timeout_scale`, the t/ counterpart
of `run-roast-test.sh`'s `per_file_timeout`: an integer multiplier on
`MUTSU_T_TIMEOUT` for a file whose cost is measured and understood. A
multiplier rather than a fixed budget, because each caller sets its own base
for its binary (30s default, 60s release, 90s debug CI). This file gets 3x.

While triaging, an unrelated laziness gap turned up and was filed as #11098:
`my @a = 1, {last if $_ >= 5; $_+1} ... *` leaves `@a` eager in mutsu, where
Rakudo keeps it lazy.
