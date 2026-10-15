# Give the param-hash reader hammer test the same stress budget as its array twin

The nightly `jit-stress-tap` job failed on
`t/concurrency/concurrent-param-hash-reader-families.t` (exit 124, "planned 32
ran 29", every reported assertion ok, no crash report). The file is a deliberate
eight-thread hammer: it runs in 27-29s alone on a debug binary with the JIT forced
hot, and `prove -j4` on a 4-core runner pushes it past the 90s base budget. Three
runs here all passed, so this is budget exhaustion, not a hang or a crash.
`scripts/run-t-test.sh` already scaled the array twin by x3 (#13344); the hash
twin now shares that case.
