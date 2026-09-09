# Compiled subtest dispatch no longer breaks Cro HTTP middleware

Issue #7552 tracked a regression found while evaluating the compiled-first
dispatch used by PR #6499. On the issue's reference main, the compiled closure
path made 16 of `Cro::HTTP`'s 24 `t/http-middleware.rakutest` subtests fail,
while the AST carrier passed them all. The failures were concentrated in
middleware callbacks that crossed an asynchronous supply and in middleware
that emitted an early response.

## What changed

The closure VM now keeps a bare block's `$/` match state scoped to the block
frame and publishes the block's final match to its defining scope at closure
exit. The changes landed as the `ec22de461` through `7c56cc194` closure-match
fix series. This prevents a nested callback or routine from replacing the
match state that the middleware block's surrounding lexical scope owns.

## Verification

On current `origin/main` (`d7a690609`), temporarily selecting the compiled-first
subtest path and running the exact Issue #7552 reproduction passes all 24
subtests. The normal `subtest` path remains the AST carrier for now: restoring
compiled-first dispatch is ADR-0047's P4 follow-up after its registry rollback
is removed in P3, and is not part of this resolved regression.

Closes #7552.
