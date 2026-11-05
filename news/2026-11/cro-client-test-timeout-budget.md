# Cro client round-trip test gets a scaled timeout budget

The nightly JIT stress TAP job went red (#15723) because
`t/concurrency/cro-client-repeated-await-response-lexical.t` hit the 90s
per-file timeout on the debug binary under `prove -j4`. The file passes with the
JIT forced hot; it spends about 53 of its 72 seconds just loading the vendored
Cro::HTTP stack on an opt-level-0 build. `scripts/run-t-test.sh` now gives it a
3x budget, like the other measured-heavy concurrency tests.
