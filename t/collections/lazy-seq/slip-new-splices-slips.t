use Test;

# From Rakudo::Options: `Slip.new($exe, Empty, Empty, $program)` builds a
# command line, and the Empty/slip arguments splice away (t/03-run).
plan 7;

is Slip.new(1, Empty, 2).elems, 2, 'Empty splices away';
is Slip.new(1, slip(3, 4), 2).join(','), '1,3,4,2', 'a slip splices in';
is Slip.new(1, (2, 3), [4, 5]).elems, 3, 'lists and arrays stay single elements';
is Slip.new((1, 2).Seq).elems, 1, 'a Seq stays one element';
is Slip.new(1, 2, 3).join(','), '1,2,3', 'plain values';
my $e = Empty;
is Slip.new(1, $e, 2).elems, 2, 'Empty held in a variable splices too';
class A {
    has $.x;
    submethod TWEAK { $!x := Empty }
    method m { Slip.new(1, $!x, 2) }
}
is A.new.m.elems, 2, 'an attribute bound to Empty splices';
