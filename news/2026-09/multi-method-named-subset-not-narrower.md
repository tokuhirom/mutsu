# Multi-method dispatch no longer counts a named parameter's subset type as narrowness

`pick_method_winner` counted `where`/subset constraints on named parameters when ranking tied
`multi method` candidates, so `multi method g(UInt:D $size = 1)` tied with
`multi method g(UInt:D :$size = 1)` and the explicit-named preference picked the named one
(Rakudo picks the positional). Only positional parameters count now, matching the sub dispatch
(`candidate_specificity_rank_for_args`). Fixes #10316's repro (`Statistics::Distributions`).

Known remaining divergence, outside this fix: for plain `Int` (no subset) mutsu's sub dispatch
picks the optional-positional candidate over a named-only one where Rakudo picks the named one.
