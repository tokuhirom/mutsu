use Test;

plan 9;

# `Nil` stringifies to "", but smartmatching against the Nil type accepts only
# Nil (and a Failure), not the empty string, array or hash.

nok ('' ~~ Nil), "'' does not match Nil";
nok ([] ~~ Nil), '[] does not match Nil';
nok ({} ~~ Nil), '{} does not match Nil';
nok (0 ~~ Nil), '0 does not match Nil';
nok (Any ~~ Nil), 'Any does not match Nil';
ok (Nil ~~ Nil), 'Nil matches Nil';

sub json-ish($v) {
    given $v {
        when Nil { 'null' }
        when Str { "S:$v" }
        default  { 'other' }
    }
}
is json-ish(''), 'S:', "given '' reaches the Str branch";
is json-ish('x'), 'S:x', 'a non-empty Str reaches the Str branch';
is json-ish(Nil), 'null', 'Nil reaches the Nil branch';
