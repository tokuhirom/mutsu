# Text::Markdown reaches ecosystem parity

Locked through the ecosystem distribution roulette on [#7884](https://github.com/tokuhirom/mutsu/issues/7884),
`Text::Markdown` 1.1.1 moved from partial (3/5 baseline files, 58/206
assertions) to green (5/5 files, 206/206 assertions).

Its inline parser uses direct positional capture reads such as `~$0` and
`~$1` after `s///`. mutsu exposed those captures through `$/` interpolation
and subscripting, but did not publish the numbered or named capture variables
themselves. The substitution VM now publishes those captures exactly as an
ordinary regex match does.

The behavior is pinned by
`t/regex/subst/subst-match-positional-captures.t`, including the Markdown
link-with-underscore reduction. The complete upstream suite passes unchanged
under both Rakudo and mutsu.
