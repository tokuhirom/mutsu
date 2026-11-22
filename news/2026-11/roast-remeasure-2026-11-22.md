# roast full re-measure, 2026-11-22

After the roast re-vendor to upstream 7639d08, `scripts/roast-history.sh` ran the
whole suite on a release build (commit 14474c6a): 1267 / 1288 files and 194545 /
195248 subtests pass. Four whitelisted files fail only in this environment: three
are listed in `ci/known-env-failures.toml`, and `S03-buf/write-int.t` passes
standalone. The whitelist stands at 1434 / 1455, which leaves 21 files off it, down
from 28 on 2026-10-14. All the regressions the 2026-10-14 re-measure found are fixed.

The re-measure found one new regression: `S05-mass/rx.t` now fails to compile,
because the RakuAST finalizer refuses regex literals with value-taking adverbs
(`:i(1)`, `:1i`, `:s(0)`), `:!` on a quantifier, `|&`, or an Nd-digit
backreference. That is filed as #18713. `TODO_roast/BLOCKERS.md` carries the new
figures, measured against Rakudo v2026.11.
