use Test;

# The free-function forms `floor(x)`/`ceiling(x)` returned a Num for a Num
# argument, unlike the `.floor`/`.ceiling` METHOD forms (which already
# returned Int correctly). A `UInt:D`/`Int:D`-typed named parameter fed the
# result then failed its own type check downstream — found via the Graph
# distribution's `Graph::MinCuttish.find-minimum-cut(method => 'karger-stein')`,
# whose `UInt:D :$th` is computed as `ceiling(1 + $n / sqrt(2))`.

plan 10;

is floor(3.2).WHAT, Int, 'floor(Num) returns an Int';
is ceiling(3.2).WHAT, Int, 'ceiling(Num) returns an Int';
is floor(3.2), 3, 'floor(3.2) == 3';
is ceiling(3.2), 4, 'ceiling(3.2) == 4';
is floor(-3.2), -4, 'floor(-3.2) == -4';
is ceiling(-3.2), -3, 'ceiling(-3.2) == -3';

# The actual failure shape: a UInt:D-typed named parameter fed the function
# form's result must type-check.
sub f(UInt:D :$th = 2) { $th }
is f(th => ceiling(1 + 10 / sqrt(2))), 9,
    'ceiling() result satisfies a UInt:D-typed named parameter';

# The method form already worked; pin it alongside so a future regression in
# either form is caught by the same file.
is (3.2).floor.WHAT, Int, '.floor method form returns an Int';
is (3.2).ceiling.WHAT, Int, '.ceiling method form returns an Int';

# NaN/Inf stay Num (no meaningful Int rounding).
is ceiling(Inf).WHAT, Num, 'ceiling(Inf) stays a Num';

done-testing;
