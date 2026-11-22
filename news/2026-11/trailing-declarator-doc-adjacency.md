# A trailing `#=` documents only an adjacent declaration

A `#=` comment kept attaching to the previous declaration however far below
it it sat, so a second `#=[...]` block after a blank line was appended to the
class above (`A.WHY` read "trailing A more for A, second block"). Rakudo
requires the comment to stay next to its declarand line by line: on the line
the declaration ends on or the one after, or right after the line the
previous `#=` of the same declaration starts on. A blank line, an ordinary
comment or a multi-line `#=[...]` block cuts the chain. Routines and grammar
rules are the exception: blank lines do not cut them off.

mutsu now applies the same rule, and a `#=` that documents nothing gets
rakudo's "Missing declarand for trailing declarator doc" worry instead of
being silently dropped or misattached. Sigilless (`my \x = ...`) and grouped
(`my ($a, $b) = ...`) declarations are recorded as declarands too, so a
`#=` after them is not reported as missing (#18096).
