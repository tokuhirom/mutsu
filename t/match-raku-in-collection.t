use Test;

plan 6;

# A Match object embedded in a collection renders via its full `Match.new(...)`
# form under `.raku`, the same as a direct `$/.raku`. Previously a Match inside
# a List/Array/Pair stringified to its matched text instead.

"hello" ~~ /(.)(.)/;

is $/.list.raku,
   '(Match.new(:orig("hello"), :from(0), :pos(1)), Match.new(:orig("hello"), :from(1), :pos(2)))',
   '$/.list.raku renders each Match in full';

is $/.caps.raku,
   '(0 => Match.new(:orig("hello"), :from(0), :pos(1)), 1 => Match.new(:orig("hello"), :from(1), :pos(2)))',
   '$/.caps.raku renders Pair values as Match.new';

my @m = "hello" ~~ /(.)(.)/;
is @m.raku,
   '[Match.new(:orig("hello"), :from(0), :pos(2), :list((Match.new(:orig("hello"), :from(0), :pos(1)), Match.new(:orig("hello"), :from(1), :pos(2)))))]',
   'an Array holding a Match renders it in full';

# A direct Match .raku is unchanged.
is $0.raku, 'Match.new(:orig("hello"), :from(0), :pos(1))', 'direct $0.raku unchanged';

# Round-trips: the collection .raku output EVALs back to Match objects.
my @r = $/.list.raku.EVAL;
is @r.elems, 2, 'collection Match .raku round-trips (count)';
is @r[0].^name, 'Match', 'round-tripped elements are Match objects';
