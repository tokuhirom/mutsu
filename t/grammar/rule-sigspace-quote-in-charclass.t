use Test;

plan 3;

# A quote character inside a character class is a class member, not the
# start of a quoted literal, so the rule's significant whitespace after it
# still becomes <.ws>.
grammar A { rule TOP { '"' <-["]>* '"' } }
is ~A.parse('"a" '), '"a" ', 'trailing sigspace kept after a <-["]> class';

grammar B {
    rule TOP { x <s> y }
    rule s { [ '"' <-["]>* '"' ] || [ "'" <-[']>* "'" ] }
}
is ~B.parse('x "a" y')<s>, '"a" ', 'subrule with a quote class keeps its trailing <.ws>';
is ~B.parse(q{x 'a' y})<s>, q{'a' }, 'single-quote class branch too';
