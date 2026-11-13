use nqp;
use Test;

plan 12;

# The list nqp ops on a plain IterationBuffer (#17337): atpos, push,
# setelems, elems and shift, as ParaSeq's batch processors run them.

my $b := nqp::create(IterationBuffer);
nqp::push($b, $_) for 10, 20, 30;
is nqp::elems($b), 3, 'push grows the buffer';
is nqp::atpos($b, 1), 20, 'atpos reads an element';
is nqp::atpos($b, -1), 30, 'a negative index counts from the end';
ok nqp::isnull(nqp::atpos($b, 7)), 'atpos past the end is null';

is nqp::push($b, 40), 40, 'push answers the pushed value';

nqp::setelems($b, 2);
is nqp::elems($b), 2, 'setelems shrinks';
is nqp::atpos($b, 1), 20, 'setelems keeps the leading elements';

nqp::setelems($b, 4);
is nqp::elems($b), 4, 'setelems grows';
is $b.List.raku, '(10, 20, Mu, Mu)', 'grown slots read back as Mu at the HLL level';

nqp::setelems($b, 0);
is nqp::elems($b), 0, 'setelems to 0 empties the buffer';
nqp::push($b, 'x');
is nqp::shift($b), 'x', 'push after setelems 0 starts from the front';

# A buffer reached through a captured variable (a closure cell).
my $out := nqp::create(IterationBuffer);
my &add = -> $v { nqp::push($out, $v) };
add($_) for ^5;
is nqp::elems($out), 5, 'push through a captured buffer';
