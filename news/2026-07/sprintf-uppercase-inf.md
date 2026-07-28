# Uppercase infinity for `%G`

`sprintf` now renders positive and negative infinity as `INF` and `-INF` for
the uppercase `%G` directive, while `%g` continues to produce `Inf` and
`-Inf`. This fixes the final two failures in `S32-str/sprintf.t`, which is now
included in the roast whitelist.

The blocker ledger had attributed the remaining failures to zero-padded
scientific `%g` formatting. Re-measurement showed that those cases already
passed in both mutsu and Rakudo, so the stale diagnosis was removed.
