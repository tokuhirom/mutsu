# The site e2e tests no longer rely on an undeclared variable

`site/e2e.test.mjs` read `$shared` and `$x` without declaring them and expected
`False` from `.defined`. mutsu now reports an undeclared variable as an error,
as Rakudo does, so the playground-isolation and REPL-reset checks failed in the
`wasm-e2e` job. That job is skipped on PRs that touch no site or WASM input, so
the stale expectation surfaced only on the v0.26.0 tag build.

The playground check now declares `our $shared` and reads `$GLOBAL::shared`
afterwards; the REPL reset check probes `$x` through `EVAL`, which sees the
variable while the session holds it and fails once the session is reset.
