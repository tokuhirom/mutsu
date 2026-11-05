use v6;
use Test;

plan 8;

# Placeholder variables and positional captures are carried through the
# executing finalization (ADR-15760 slice 2) and keep their meaning.

is { $^a + $^b }.(1, 2), 3, 'scalar placeholders bind positionally';
is { $^b ~ $^a }.(1, 2), '21', 'placeholders sort alphabetically';
is { @^l.elems }.([1, 2, 3]), 3, 'an array placeholder';
is { %^h<k> }.({ k => 7 }), 7, 'a hash placeholder';
is { &^cb(4) }.(* * 2), 8, 'a code placeholder';
is { $:n * 2 }.(:n(5)), 10, 'a named placeholder';

"abc" ~~ /(b)(c)/;
is "$0$1", 'bc', 'positional captures in a string';
is ($0 ~ $1), 'bc', 'positional captures as terms';
