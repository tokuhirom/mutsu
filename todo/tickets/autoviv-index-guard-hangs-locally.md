# `t/autoviv-index-guard.t` hangs (exit 124) on this dev container, unrelated to any specific PR

Found while running `make test` locally to validate an unrelated fix
(investigating `todo/deep/attribute-bind-severed-by-unrelated-later-call.md`,
which turned out to already be resolved by a concurrent session's work —
see that file's own note).

`timeout 30 target/debug/mutsu t/autoviv-index-guard.t` prints only `1..13`
and then hangs until `timeout` kills it (exit 124) -- deterministically,
5/5 runs, both on `main` at `78a123190` and at `origin/main` (`6878a4c03`).
Debug build only (not yet checked against a release build).

This is almost certainly *not* a general regression: `main`'s CI (`gh run
list --branch main`) is green, and CI's `test` job runs this exact file
(`prove t/`, debug binary per `docs/adr/0014-...`). So the hang is either:
- environment-specific to this container (e.g. a memory/cgroup limit this
  dev LXC has that CI's runner does not, making the "absurd index" guard in
  `t/autoviv-index-guard.t` (added in #3687, "guard array autoviv & string
  repeat allocations") take a slow real path instead of failing fast), or
- newly flaky/load-sensitive and CI has gotten lucky so far.

Not investigated further -- out of scope for the session that surfaced it.
Next step for whoever picks this up: reproduce with a release build
(`target/release/mutsu`), and if it still hangs, `rust-gdb` a running process
to see which allocation-guard check (`@a[9999999999999] = 1`-shaped) is
looping instead of failing fast, per CLAUDE.md's debugging guidelines.
