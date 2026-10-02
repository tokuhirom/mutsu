# The battery gate honours the flaky quarantine, and Crypt::Random 03-uniform.t is its first entry

The bundled-library release gate (`scripts/battery-testsuite.sh`) failed once on
PR #10871 with `Crypt::Random 03-uniform.t FAIL(ok=1/2,notok=1)` and passed on a
rerun (#10885). The suspicion was a bad `/dev/urandom` read. Measurement says
otherwise. 20,000 draws of `crypt_random_uniform(10000)` in one process gave 3
neighbour collisions (about 2 expected) and 8,657 distinct values (about 8,647
expected), with none out of range. A byte histogram of `crypt_random_buf` was
flat, and 3,400 runs of the file, eight at a time on four cores, all passed. A
short read cannot cause the failure either: `Crypt::Random::Nix` dies on one
instead of returning a value.

The test fails by design. It asserts `$one != $two && $two != $thr` over three
draws from `^10000`, which a correct RNG violates about 2 times in 10,000. The
gate runs on every CI run, so an occasional red is expected.

The gate had no way to express that, so `flaky-tests.txt` now accepts
`battery:<Name>/<file>` entries. `make check-flaky-list` checks that each one
names a `batteries-whitelist.txt` row, and its new `--self-test` pins that
rule. The gate re-rolls a listed file with the same rules as
`scripts/flaky-retry.sh`: at most three attempts, no retry after a signal
death, and every retry logged. CI reports those retries in a step of their own.
The gate also prints the failing `not ok` lines and the tail of the output for
any failed file. The next failure will therefore say which assertion failed,
not just how many.
