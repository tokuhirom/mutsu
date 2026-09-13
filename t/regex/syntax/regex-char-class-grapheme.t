use Test;

# Raku strings are NFG, so a character class and a Unicode property both test a
# whole GRAPHEME. Two consequences, and mutsu had neither (tokuhirom/mutsu#8342):
#
#   1. A class or property may not START inside a cluster. The combining mark in
#      "a\x[094D]b" is part of the first grapheme, not a position any atom can
#      begin at, so `/<:Mn>/` and `/<[\x[094D]]>/` both fail to match it.
#   2. A class may HOLD a multi-codepoint grapheme. `<[क्ष]>` is one entry, and
#      mutsu rejected it outright: "Cannot use क् as a range endpoint, as it is
#      not a single codepoint" -- conflating a legal class *entry* with an
#      illegal range *endpoint*.
#
# They are one fix: (1) alone breaks what (2) makes representable. In particular
# `\c[A,B]` names one grapheme, and while it was stored as two separate
# codepoints it could only match by starting inside the cluster --
# `roast/S05-mass/named-chars.t` test 155 is exactly that case.

plan 12;

# --- (1) no atom may start inside a cluster --------------------------------

nok "a\x[094D]b" ~~ /<:Mn>/,
    'a property does not match a combining mark inside a grapheme';
nok "a\x[094D]b" ~~ /<[\x[094D]]>/,
    'nor does a class holding that mark';
nok "\x[1ea2]\x[5b4]" ~~ /<[\x[5b4]]>/,
    'nor when the mark is the last codepoint of the string';

# The mark IS matchable when it is a grapheme of its own: UAX #29 GB4 says
# nothing extends a control, so the mark after "\n" starts its own cluster.
ok "a\n\x[094D]" ~~ /<:Mn>/,
    'a mark after a control is its own grapheme and does match';

# --- the base of a cluster is not the cluster ------------------------------

nok "a\x[301]" ~~ /<[a]>/,
    'a class entry does not match the base of a synthetic grapheme';
ok "a\x[301]b" ~~ /<[b]>/,
    '...but an ordinary grapheme after one still matches';

# --- (2) a class entry may be a multi-codepoint grapheme -------------------

is "क्ष".chars, 1, 'the test cluster really is one grapheme';
ok "क्ष" ~~ /<[क्ष]>/, 'a class holds a multi-codepoint grapheme';
ok "क्ष" ~~ /^<[क्ष]>$/, '...and consumes the whole of it';
nok "क" ~~ /^<[क्ष]>$/, '...and does not match just its base';

# The `\c[NAME,NAME]` spelling of the same thing, which is what
# roast/S05-mass/named-chars.t exercises.
ok "\x[1ea2]\x[5b4]"
    ~~ /<[\c[LATIN CAPITAL LETTER A WITH HOOK ABOVE,HEBREW POINT HIRIQ]]>/,
    'a named multi-codepoint grapheme works as a class entry';

# --- a cluster is still not a legal range endpoint -------------------------

{
    my $err;
    try { EVAL 'so "x" ~~ /<[क्ष..z]>/' };
    ok $!.defined, 'a cluster is still rejected as a range endpoint';
}
