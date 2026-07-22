use v6;
use Test;

plan 9;

# PLAN §8.12: reduce with a destructuring reducer signature. The accumulator
# tuple binds through the first parameter's sub-signature.

sub count-and-sum-evens( (Int \count, Int \sum), Int \x ) {
    x %% 2 ?? (count + 1, sum + x) !! (count, sum)
}
is-deeply (reduce &count-and-sum-evens, (0, 0), |(1, 2, 3, 4)).List, (2, 6),
    'reduce with a destructuring first param and a seed tuple';

# A destructuring parameter consumes exactly ONE positional argument.
sub f( ($c, $s), $x ) { ($c + $x, $s) }
is-deeply f((0, 1), 5), (5, 1), 'destructure followed by a plain param';
sub g( (Int \c, Int \s), Int \x ) { (c + x, s) }
is-deeply g((0, 1), 5), (5, 1), 'sigilless typed destructure followed by a plain param';
sub sole( ($c, $s) ) { ($c, $s) }
is-deeply sole((7, 8)), (7, 8), 'sole destructuring param still binds';
sub tail( $x, ($c, $s) ) { ($x, $c, $s) }
is-deeply tail(1, (7, 8)), (1, 7, 8), 'destructure in the last position still binds';

# The `|(...)` anonymous capture still consumes all remaining arguments.
sub cap(| ($a, $b)) { "$a $b" }
is cap(3, 4), '3 4', 'anonymous capture subsignature still takes all args';
sub ncap(|c ($a, $b)) { "$a $b" }
is ncap(5, 6), '5 6', 'named capture subsignature still takes all args';

# A sigilless param declared inside a sub-signature shadows builtin listops
# in the body (`sum + x` reads the param, not `sum(+x)`).
sub shadows( (\sum, \x) ) { sum + x }
is shadows((10, 5)), 15, 'sub-signature sigilless param shadows the sum builtin';

# reduce's one-arg rule: several arguments each stay one item.
is-deeply (reduce -> $a, $b { $a + $b }, (1, 2, 3)), 6,
    'a single list argument still flattens into the item list';

done-testing;
