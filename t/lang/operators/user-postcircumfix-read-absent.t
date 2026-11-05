use v6;
use Test;

# With no user `postcircumfix` routine anywhere, a subscript read goes straight
# to the native read (the per-read candidate probe is skipped, #15394); a
# candidate that appears later is still honoured.
plan 7;

my @a = <a b c>;
is @a[1], 'b', 'an Array element read';
my %h = k => 1, j => 2;
is %h<k> + %h{'j'}, 3, 'Hash element reads';
class P { has @.items = <x y z>; method AT-POS($i) { @!items[$i] } }
is P.new[2], 'z', 'an instance read through AT-POS';
my $sum = 0;
$sum += (10, 20, 30)[$_] for ^3;
is $sum, 60, 'a loop of List reads';

class R is Array { }
my @r is R = <a b c d e>;
is @r[1], 'b', 'a read of an Array subclass without a candidate';

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
    is @z[1.5], 'c', 'a candidate declared later decides the element read';
    CODE
is @a[2], 'c', 'plain reads outside the EVAL are unaffected';
