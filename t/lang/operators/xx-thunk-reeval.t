use v6;
use Test;

# `xx` THUNKS its left side: the expression is re-evaluated for every
# repetition. HTTP::HPACK reads a header's name and value with
# `decode-str($packed, $idx) xx 2`, advancing the rw offset twice.

plan 5;

my @r = rand xx 3;
is @r.unique.elems, 3, 'rand xx 3 yields three different numbers';

sub read-one(Blob $b, int $o is rw) { $o++; $b[$o - 1] }
my Blob $data = Blob.new(10, 20, 30);
my int $idx = 0;
my ($first, $second) = read-one($data, $idx) xx 2;
is $first, 10, 'first repetition reads the first byte';
is $second, 20, 'second repetition re-runs the call';
is $idx, 2, 'the rw offset advanced twice';

my $n = 7;
is-deeply ($n xx 2).List, (7, 7), 'a plain variable still value-repeats';
