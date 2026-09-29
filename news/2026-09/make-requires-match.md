# `make` requires a Match in `$/` and sets `$/.made`

`make "x"` outside a successful match used to succeed silently. It now throws
`X::Make::MatchRequired` (`The make function expects $/ to contain a Match, but it
contains Nil`), like Rakudo (`Type/Match.rakudoc`).

The check reads the caller frame's `$/` the same way the `$/` variable does — the
local slot first (an action method's `$/` parameter), then env — through one shared
helper that `$<name>` now uses too. Reading env `/` alone would have seen the Nil a
nested failed `.subst` leaves there inside an action, which broke the first attempt.
`make` also stores its value on that Match, so `"a" ~~ /a/; make 7; $/.made` is `7`
as in Rakudo. `t/regex/regex-make.t`, which asserted a top-level `make` with no match
works, was corrected; the behavior is pinned by `t/regex/match/make-requires-match.t`.
