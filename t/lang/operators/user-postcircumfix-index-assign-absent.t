use v6;
use Test;

# With no user `postcircumfix` routine anywhere, a subscript store goes
# straight to the native store (the per-store candidate probe is skipped,
# #16320); a candidate that appears later is still honoured.
plan 7;

my @a = <a b c>;
@a[1] = 'X';
is @a.join(','), 'a,X,c', 'an Array element store';
my %h;
%h<k> = 1;
%h{'j'} = 2;
is %h.sort.map(*.key).join(','), 'j,k', 'Hash element stores';
my @b;
@b[$_] = $_ * 2 for ^5;
is @b.join(','), '0,2,4,6,8', 'a loop of stores';

class R is Array { }
my @r is R = <a b c d e>;
@r[1] = 'Q';
is @r.join(','), 'a,Q,c,d,e', 'a store into an Array subclass without a candidate';

EVAL q:to/CODE/;
    my constant &old-same = &postcircumfix:<[ ]>;
    proto sub postcircumfix:<[ ]>($, |) {*}
    multi sub postcircumfix:<[ ]>(R:D \SELF, Any:D \index) {
        old-same SELF, index.round
    }
    multi sub postcircumfix:<[ ]>(R:D \SELF, Any:D \index, |c) {
        old-same SELF, index.round, |c
    }
    my @z is R = <a b c d e>;
    @z[1.5] = 'X';
    is @z.join(','), 'a,b,X,d,e', 'a candidate declared later decides the element';
    @z[0] = 'Z';
    is @z.join(','), 'Z,b,X,d,e', 'and an integer index is unchanged';
    CODE
@a[0] = 'Y';
is @a.join(','), 'Y,X,c', 'plain stores outside the EVAL are unaffected';
