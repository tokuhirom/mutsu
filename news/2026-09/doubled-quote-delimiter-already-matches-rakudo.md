# `Q[[...]]` was never wrong: a doubled quote delimiter matches rakudo

A ticket reported that `Q[[1]]` "drops the leading nested bracket", yielding `1`
where rakudo supposedly yields `[1]`. Re-measured against real `raku`: it does
not. Rakudo yields `1` too, and `Q[[1] 2]` is a syntax error in rakudo just as it
is in mutsu.

The reading the ticket assumed — a single `[` delimiter with a nested pair as the
first content character — is not how Raku parses that source. A *repeated*
bracket is one delimiter: `[[` opens and `]]` closes, and nesting is counted in
units of the whole repeated run, not of the single bracket. So `Q[[1]]` is the
doubled-delimiter quote of `1`; `Q[[a[[b]]c]]` keeps its nested doubled pair
(`a[[b]]c`); and `Q[[a[b]]]` is a syntax error, because a lone `[` can never
balance a `[[`. mutsu already implemented all of this — `count_repeated_bracket`
plus `read_multi_bracketed` in `src/parser/primary/string/` — and agreed with
rakudo on every spelling checked: all four ASCII bracket pairs, doubled and
tripled runs, `q`/`qq`/`Q`, and the error cases.

No code change was needed. What the finding was actually worth is a pin, so the
correct reading cannot later be "fixed" into a regression by someone reading the
same report: `t/quote-doubled-delimiter.t`, 25 assertions taken from rakudo,
passing under both `raku` and mutsu.

## The one real divergence found while checking

Sweeping the family turned up a genuine, much narrower bug that the ticket had
not noticed, recorded as `todo/tickets/qq-doubled-delimiter-interpolation-subscript.md`:
`qq[[@a[0]]]` fails to parse in mutsu where rakudo prints `1`. mutsu scans for the
closing `]]` textually before interpolating, so the subscript's `]` abutting the
close is mistaken for the delimiter; rakudo's quote grammar parses the
interpolation atom first and only then looks for the close. It is a separate
mechanism from the delimiter counting this ticket was about, and it only shows up
when a subscript ends exactly at the closing run — `qq[[x@a[0]y]]` and
`qq{{@a[0]}}` are both fine today.
