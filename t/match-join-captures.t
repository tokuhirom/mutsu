use v6;
use Test;

# Match.join joins the POSITIONAL CAPTURES (`.list`), not the matched string.
# Crypt::Random::Extra builds a UUID with
#   ($hex ~~ /(........)(....)(....)(....)(............)/).join("-")

plan 4;

my $s = '0123456789abcdef0123456789abcdef';
is ($s ~~ /(........)(....)(....)(....)(............)/).join('-'),
    '01234567-89ab-cdef-0123-456789abcdef',
    'Match.join joins the positional captures';

is ('abcd' ~~ /(ab)(cd)/).join('|'), 'ab|cd', 'two captures join';
is ('abcd' ~~ /ab/).join('-'), '', 'a captureless Match joins to the empty string';
is ('abcd' ~~ /(a)(b)(c)/).join(''), 'abc', 'empty separator concatenates captures';
