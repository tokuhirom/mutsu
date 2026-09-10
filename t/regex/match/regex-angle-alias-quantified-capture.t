use Test;

plan 10;

# An angle-bracket alias of a character class or Unicode property
# (`<name=[...]>`, `<name=:Prop>`) is quantified *per iteration* like a builtin
# subrule, so `$<name>` is a List with one Match per repetition — NOT a single
# Match spanning the whole run. (The sigil-prefix alias `$<name>=...` is the
# opposite: it captures the whole quantified span as one Match.)
# roast S05-metasyntax/angle-brackets.t test "=[...] renaming worked".

# Char-class alias, quantified -> list of per-iteration matches.
ok 'foobar' ~~ / <foo=[bao]>+ /, 'angle char-class alias matches';
is $<foo>.elems, 4, 'quantified angle char-class alias yields a List';
is $<foo>.join(','), 'o,o,b,a', 'each repetition is a separate capture';

# Unicode-property alias, quantified -> list.
ok 'abc' ~~ / <ltr=:Letter>+ /, 'angle property alias matches';
is $<ltr>.elems, 3, 'quantified angle property alias yields a List';

# Non-quantified angle char-class alias -> single Match (unchanged).
ok 'b' ~~ / <one=[bao]> /, 'single angle char-class alias matches';
is ~$<one>, 'b', 'non-quantified alias is a single Match';

# The sigil-prefix alias on a quantified atom stays a single whole-span Match.
ok 'abc' ~~ / $<all>=\w+ /, 'sigil alias matches';
is ~$<all>, 'abc', 'quantified sigil alias captures the whole span';

# A negated char-class alias also lists per iteration.
'foobar' ~~ / <fb=-[aeiou]>+ /;
is $<fb>.elems, 1, 'negated char-class alias lists per iteration (leading f)';
