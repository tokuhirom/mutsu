use v6;
use Test;

# The candidates of a `multi token` that declare the same positional names
# share one match-time parameter window (ADR-12917): whichever candidate the
# dispatcher picks reads its parameter when the cursor reaches it.

plan 5;

grammar H {
    token TOP { <p('a')> <p(5)> }
    multi token p(Str $s) { $s+ }
    multi token p(Int $s) { \d+ }
}
is H.parse("aa123").Str, "aa123", 'candidates picked by argument type';
is-deeply H.parse("aa123")<p>.map(*.Str).List, ("aa", "123"), 'each call matched its own text';

grammar I {
    token TOP { <q('x')> <q(7)> }
    multi token q(Str $v) { $v $v }
    multi token q(Int $v) { $v '!' }
}
is I.parse("xx7!").Str, "xx7!", 'parameter read inside each candidate';
nok I.parse("xx7?").defined, 'a mismatch still fails';

grammar J {
    token TOP { <r('ab')> <r('cd')> }
    multi token r(Str $v) { $v }
}
is J.parse("abcd").Str, "abcd", 'a single multi candidate with a parameter';
