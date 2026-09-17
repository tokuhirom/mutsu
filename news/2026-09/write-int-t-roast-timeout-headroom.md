# write-int.t gets its own roast per-file timeout budget

`test-suites` on `main` went red (commit `7a80c13c`, CI run 35159960371) with
`roast/S03-buf/write-int.t` "Dubious, test returned 124 (wstat 31744, 0x7c00)"
and "Failed 377/2530 subtests" under `make roast`'s `prove -j4` — a timeout,
not a real assertion failure. The same job's own immediate serial re-run of
the file passed all 2530 subtests, and neither the file's content (2530
subtests exhaustively covering `write-{u,}int{8,16,32,64,128}` across 3
endiannesses, generated from an unchanged `1,2,4,8,16` byte-width list) nor
`src/builtins/buf_write_int.rs` had changed recently.

Measured the margin directly: the CI job's own serial re-run clocked this
file at ~23 wallclock seconds running alone — already 77% of the
`scripts/run-roast-test.sh` default 30s per-file budget with zero
contention. Reproduced the shape locally (release build): ~10.5s solo,
~12.8s under synthetic 4-core CPU saturation (+22%). Scaling the CI-runner's
23s solo time by that same +22% lands right at ~28s, just under the 30s
timeout — so an unlucky roll of `prove -j4` scheduling was always enough to
tip it over. This is the same "slow file, thin default-timeout margin"
shape already documented for `S04-declarations/state.t`,
`S04-exception-handlers/catch.t`, `S04-exceptions/exceptions-alternatives.t`
and `S03-sequence/exhaustive.t` in that script — `write-int.t` just hadn't
hit the lottery yet.

Gave it an explicit 90s budget in `scripts/run-roast-test.sh`, matching the
existing pattern for this class of file. No interpreter change; the fix is
scheduling headroom, not a functional regression.
