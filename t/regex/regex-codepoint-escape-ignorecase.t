use Test;

# A codepoint escape names exact codepoints, which `:i` does not fold; a quoted
# string or a character class holding the same escape is folded (#18777).

plan 16;

nok "A" ~~ /:i \x[61]/,          ':i does not fold \x[61]';
nok "A" ~~ /:i \x61/,            ':i does not fold \x61';
nok "AB" ~~ /:i \x[61,62]/,      ':i does not fold a \x[...] run';
nok "A" ~~ /:i \o[141]/,         ':i does not fold \o[141]';
nok "A" ~~ /:i \c[LATIN SMALL LETTER A]/, ':i does not fold \c[NAME]';
ok  "A" ~~ /:i \X[61]/,          ':i does not fold the negated \X[61]';
ok  "A" ~~ /:i \C[LATIN SMALL LETTER A]/, ':i does not fold the negated \C[NAME]';
nok "A" ~~ m:i/\x61/,            'm:i does not fold \x61';
nok "xA" ~~ /x [:i \x61]/,       'a scoped :i does not fold \x61';
is ~("Aa" ~~ /:i a \x61/), 'Aa', 'the atoms around the escape still fold';
is ~("AAa" ~~ /:i a \x61+/), 'Aa', 'a quantified escape stays exact';
is ~("AB" ~~ /:i "\x[61,62]"/), 'AB', 'a quoted escape is folded';
is ~("A" ~~ /:i <[\x61]>/), 'A', 'a character class is folded';
is ~("a" ~~ /:i \x61/), 'a', 'the exact codepoint still matches';
is ~("abab" ~~ /\c[LATIN SMALL LETTER A, LATIN SMALL LETTER B]**2/), 'abab',
    'a \c[...] list is one atom';
is ~("abab" ~~ /\x[61,62]**2/), 'abab', 'a \x[...] list is one atom';
