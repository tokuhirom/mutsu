use Test;

plan 10;

# A character-class entry whose case fold expands to multiple characters still
# matches the original grapheme, but not a partial fold.
ok "\x[DF]" ~~ /:i <[ \x[DF] ]>/,
    'a sharp-s escape in a class matches itself under :i';
nok 's' ~~ /:i <[ \x[DF] ]>/,
    'a sharp-s escape does not match part of its fold';
ok "\c[LATIN SMALL LETTER SHARP S]" ~~ /:i <[ \c[LATIN SMALL LETTER SHARP S] ]>/,
    'a named sharp-s class entry matches itself under :i';
ok "\c[LATIN SMALL LIGATURE FF]" ~~ /:i <[ \c[LATIN SMALL LIGATURE FF] ]>/,
    'a named ligature class entry matches itself under :i';
nok 'f' ~~ /:i <[ \c[LATIN SMALL LIGATURE FF] ]>/,
    'a ligature class entry does not match part of its fold';
ok "a\x[308]" ~~ /:i <[ a \x[308] ]>/,
    'a base plus combining escape forms one class entry';
nok 'a' ~~ /:i <[ a \x[308] ]>/,
    'a composed class entry does not match its bare base';
ok 'm' ~~ /:i <[ \x[4D] ]>/,
    'a single-character class escape still folds normally';
nok 'm' ~~ /:i <-[ \x[4D] ]>/,
    'a negated single-character class still rejects the other case';
ok 'n' ~~ /:i <-[ \x[4D] ]>/,
    'a negated single-character class still accepts another character';
