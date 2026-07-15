use v6;
use Test;

plan 6;

# A derived grammar that adds a proto-token candidate must keep the base
# grammar's candidates (advent2009-day24).

grammar B {
    rule TOP { <statement> }
    proto token statement { <...> }
    rule statement:sym<expr> { \d+ }
}
grammar D is B {
    rule statement:sym<repeat> { 'repeat' \d+ }
}

ok D.parse('repeat 5'), 'derived candidate matches';
ok D.parse('7'), 'inherited candidate still matches in the derived grammar';
ok B.parse('7'), 'base grammar unchanged';
nok B.parse('repeat 5'), 'base grammar is not extended by the derived one';

# A derived same-sym candidate replaces (shadows) the base one — it does not
# merge with it (matches Rakudo v2026.06 behavior).
grammar D2 is B {
    rule statement:sym<expr> { 'x' \d+ }
}
nok D2.parse('9'), 'overridden base candidate no longer matches';
ok B.parse('7'), 'base grammar unaffected by the override';
