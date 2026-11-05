use v6;
use Test;

# A user `multi sub postcircumfix:<[ ]>` candidate that delegates to the CORE
# routine with a changed index (Array::Rounded's rounding) decides which
# element an assignment or a binding addresses, not only a read (#16294).
plan 4;

class R is Array { }
my constant &old-same = &postcircumfix:<[ ]>;
proto sub postcircumfix:<[ ]>($, |) is export {*}
multi sub postcircumfix:<[ ]>(R:D \SELF, Any:D \index) {
    old-same SELF, index.round
}
multi sub postcircumfix:<[ ]>(R:D \SELF, Any:D \index, |c) {
    old-same SELF, index.round, |c
}

my @a is R = <a b c d e>;
is @a[1.5], 'c', 'a read goes through the candidate';
@a[1.5] = 'X';
is @a.join(','), 'a,b,X,d,e', 'an assignment goes to the rounded index';
@a[3.4] := 'Y';
is @a.join(','), 'a,b,X,Y,e', 'a binding goes to the rounded index';
@a[0] = 'Z';
is @a.join(','), 'Z,b,X,Y,e', 'an integer index is unchanged';
