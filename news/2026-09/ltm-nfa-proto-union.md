# LTM measures a proto as the union of its candidates

A `|` branch that calls a proto (`proto token t {*}` with `t:sym<...>`
candidates) used to be measured by ranking the candidates, walking only the
winner, and keeping only the winner's longest end. Rakudo instead inlines every
candidate into the branch's NFA, so its declarative prefix can run through a
losing candidate, or through a shorter end of the winner. With
`t:sym<a> { 'a' }` and `t:sym<ab> { 'ab' }`, the alternation
`[ <t> [ 'bcde' | 'c' ] | 'abcd' ]` on `"abcde"` ranks the `<t>` branch first
in Rakudo (its prefix reaches 5 through `'a' 'bcde'`). mutsu ranked `'abcd'`
first.

The LTM NFA of ADR-0125 no longer declines a proto. It compiles a proto into a
split over every candidate body; `<sym>` needs no special handling, because it
was already rewritten to the candidate's own sym text. This takes most of the
walker cost off real grammars, which are built out of protos. The walker's
measurement of a proto changed to the same union (#9643), so the ADR-0046
ranking of an outer proto's candidates agrees too. A real match still ranks the
candidates and commits to the first one that matches.
