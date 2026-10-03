# CI builds the release binary inside `test-suites`

`ci.yml`'s separate `build` job existed to compile the release binary once for
three consumers. After the stress jobs moved to `stress.yml`, `test-suites` was
the only one left, and it already waited for the build. Keeping the split cost
a second runner slot per run, an artifact round-trip, and the queue wait
between the two jobs — 7.5 minutes on a congested run. `test-suites` now builds
the binary itself, so a PR uses four concurrent runner slots instead of five
([ADR-11581](../../docs/adr/11581-ci-runner-budget.md), amendment).
