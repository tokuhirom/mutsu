# A `CATCH` around `.tap` of a dying `supply` runs once

A `CATCH` that has already run inline at the `die` of a `supply` body stamps a
verdict on the error (ADR-0072) so the region that owns it does not run it again
while the error unwinds. `.tap` rebuilt the error from the reason value, which
dropped the stamp, so the `CATCH` ran twice and `throws-like { $s.tap }, ...`
produced four assertions where rakudo produces two. `.tap` now rethrows the
body's own error (#16030). `react` and `.Promise` consumers still rebuild it:
#16054.
