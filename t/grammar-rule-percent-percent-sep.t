use Test;

# In a grammar `rule` (which is `token` + implicit `:sigspace`), the `%%`
# modified quantifier (separator, trailing separator allowed) was destroyed by
# the implicit-whitespace injection: the second `%` of `%%` was mistaken for
# the separator atom, so `<n>+ %% ","` never matched. `%` (no trailing) and
# `%%` inside a plain `token` already worked.

plan 8;

grammar G1 { rule TOP { <num>+ %% "+" }; token num { \d+ } }
ok  G1.parse("1+2+3"),  'rule <subrule>+ %% sep matches';
ok  G1.parse("1+2+3+"), 'rule %% allows a trailing separator';
is  G1.parse("1+2+3")<num>.elems, 3, 'rule %% captures all subrule matches';

grammar G2 { rule TOP { \d+ %% "+" } }
ok  G2.parse("1+2+3"),  'rule \d+ %% sep matches';
ok  G2.parse("1+2+3+"), 'rule \d+ %% trailing separator';

# `%` (non-trailing) still works and rejects a trailing separator.
grammar G3 { rule TOP { \d+ % "+" } }
ok  G3.parse("1+2+3"),  'rule \d+ % sep matches';
nok G3.parse("1+2+3+"), 'rule % rejects a trailing separator';

# Separator with surrounding whitespace in the input (sigspace).
grammar G4 { rule TOP { <n>+ %% "," }; token n { \d+ } }
is  G4.parse("1, 2, 3")<n>.elems, 3, 'rule %% with spaced input';
