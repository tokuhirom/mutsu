# Nightly stress: budget for the two concurrency hammer tests

The nightly `gc-stress-tap` and `jit-stress-tap` jobs (#13344) went red because
`t/concurrency/concurrent-attr-hash-writes-do-not-corrupt.t` and
`t/concurrency/concurrent-param-array-reader-families.t` hit the 90s per-file
timeout on the debug binary under `prove -j4` (exit 124, bad plan, no failed
assertion, no crash report). Reproduced locally: alone on an idle box they take
49-63s and 30-36s on a debug build (3-6s on release), and pass 8/8 under CPU
load on the release binary, so this was a budget problem, not a mutsu defect.
Both files now get a x3 timeout via `per_file_timeout_scale` in
`scripts/run-t-test.sh`.
