# RakuAST round-trip ratchet: 125 passing files recorded

A `scripts/rakuast-frontend.sh causes` survey after the S10 slices (#12291,
#12295, #12308, #12312, #12313) found 125 `t/` files that already pass under
`MUTSU_RAKUAST=1` but were not in `ci/rakuast-frontend-passing.txt`. They are
listed now, so the ratchet protects them: 6148 files, `check` passes on a
fresh release build. Part of S10 of #7564.
