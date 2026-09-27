use Test;

plan 16;

# A `Match` is a `Capture`: `Capture.list` is its positional part, and Any's
# list coercions and iteration methods all go through `self.list`. None of them
# may coerce the Match to its `.Str`.
my $m = 'ab' ~~ /(.)(.)/;

is $m.List.map(~*).join(','), 'a,b', '.List is the positional captures';
isa-ok $m.Array, Array, '.Array is a real Array';
is $m.Array.elems, 2, '.Array holds the positional captures';
is $m.Slip.map(~*).join(','), 'a,b', '.Slip';
is $m.Seq.map(~*).join(','), 'a,b', '.Seq';
is $m.flat.map(~*).join(','), 'a,b', '.flat';
is $m.map(*.Str).join(','), 'a,b', '.map iterates the positional captures';
is $m.grep(*.so).elems, 2, '.grep';
is ~$m.first(*.so), 'a', '.first';
is $m.tail(1).map(~*).join, 'b', '.tail';
is $m.reverse.map(~*).join(','), 'b,a', '.reverse';
is ~$m.iterator.pull-one, 'a', '.iterator';
is $m.pairs.map(*.key.^name).join(','), 'Int,Int', 'positional .pairs keys are Ints';

# `for` over a non-itemized Match (a subscript of another Match) iterates its
# positional list; a quantified group is ONE positional (an Array).
grammar G {
    token TOP { <x> }
    token x { (.)+ }
}
my $g = G.parse('ab');
my @seen;
for $g<x> { @seen.push: .^name }
is @seen.join(','), 'Array', 'for over a Match subscript iterates its .list';

# An itemized Match stays one item.
my @one;
for $m { @one.push: .^name }
is @one.join(','), 'Match', 'for over a Match in a scalar is one iteration';

my @chars;
for $g<x> { for @$_ { @chars.push: ~$_ } }
is @chars.join(','), 'a,b', 'the quantified capture list holds each iteration';
