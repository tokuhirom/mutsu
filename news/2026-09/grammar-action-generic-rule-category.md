# Grammar actions can use generic rule-category method names

Grammar action classes may define methods such as
`method modifier:<null>($/)` for the candidates of a `proto rule modifier`.
mutsu previously rejected that declaration as an unknown operator category, and
even after parsing it could not select the shorthand method during action
dispatch.

Methods now accept the angle-qualified and guillemet-qualified generic names
used by grammar-rule candidates. Action dispatch checks the matching shorthand
name while preserving the existing `:sym<...>` and bare-adverb precedence.
Plain `sub` declarations remain subject to Rakudo's rejection for this syntax.
The regression is covered by
`t/grammar/grammar-action-generic-rule-category.t`, found while remeasuring
Red 0.2.5 for [#7988](https://github.com/tokuhirom/mutsu/issues/7988).
