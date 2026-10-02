# Remote sessions: incremental release builds and a background warm build

Two changes to `.claude/hooks/session-start.sh` cut build waiting in the ~4-core remote container.
Neither touches the local box or CI.

**Release builds are incremental for the session.** The hook exports
`CARGO_PROFILE_RELEASE_INCREMENTAL=true` through `CLAUDE_ENV_FILE`. The interpreter is one
~730k-line crate, and release is not incremental by default, so touching one file rebuilt the
whole crate. Measured on the same container:

| release build of `mutsu` | default | incremental |
|---|---|---|
| cold (dependencies included) | 10m38s | 10m53s |
| after `touch`ing one file | 500–524 s | 27–28 s |
| after editing one function body | — | 30 s |

The incremental binary ran 150 whitelisted roast files in the same time: 57.7–58.4 s, against
57.7–58.9 s for the default build. On the benchmarks it was 0–5% slower, for example
`bench-multi-dispatch` 0.84 s vs 0.83 s and `bench-regex-scan-walk` 0.44 s vs 0.42 s. Shipped
binaries come from CI, which never sees the variable. The `perf-tuning` skill now says to build
both sides of a perf PR's final wall-clock A/B with `CARGO_PROFILE_RELEASE_INCREMENTAL=false`.
`MUTSU_SETUP_NO_INCREMENTAL_RELEASE=1` opts out.

**The debug test build starts in the background.** A fresh container has no `target/`, and the
debug `cargo test` build is ~4.5 min cold. The hook now starts `scripts/dev run warm-build -- nice
-n 10 cargo test --no-run`, so that time overlaps with the session reading `AGENTS.md` and the
issue instead of following it. The build runs as a `scripts/dev` job, so `scripts/dev status`
shows it, and it runs under `nice`, so an interactive build gets the cores first. A cargo command
on the same profile waits on cargo's build lock and then reuses the result.
`MUTSU_SETUP_NO_WARM_BUILD=1` opts out.

These are the short-term measures. The structural cause — one crate that cannot be split because
`Interpreter` is a god object and the layers depend on each other — is tracked in #10779.
