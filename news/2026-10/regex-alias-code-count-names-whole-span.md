# A `$<name>=` alias on a `** { ... }` quantifier names the whole span

`"aaaa" ~~ / $<x>=a ** {2} /` bound `$<x>` once per iteration, so the match
printed two `x => ｢a｣` entries and `$<x>` was a List. Its static twin
`$<x>=a ** 2` -- and rakudo, for both -- binds the alias to the whole quantified
span as one Match (`x => ｢aa｣`).

The cause was in the regex parser, not the walk or the compiled engine. A
sigil alias on a quantified, non-grouping atom is wrapped in a non-capturing
group so the alias sits on a `One` token covering the entire run. That wrap
listed `*`, `+` and `** N..M`, but admitted the code-count form `** { ... }`
only when it also carried a `%` separator. Every unseparated code count fell
through to the per-iteration path. The wrap now covers `RepeatCode`
unconditionally, so both engines -- which consume the same token tree --
agree with rakudo. An aliased capture *group* (`$<x>=(a) ** {2}`) is still
the per-iteration List case, as in rakudo.

Pinned by `t/regex/regex-alias-repeat-code-count.t`, whose expectations are
rakudo's output for each shape (greedy, frugal, ratcheted and zero counts, a
count read from an earlier capture, `@<x>=`, `$0=`, a separator, sigspace).
