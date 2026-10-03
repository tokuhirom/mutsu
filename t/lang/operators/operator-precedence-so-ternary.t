use Test;

# `so` / `not` are loose unary prefixes: looser than the conditional `?? !!`
# and item assignment, tighter than the comma and `and`/`or` (#11478).

plan 9;

is-deeply (so 0 ?? 1 !! 0), False, '`so A ?? B !! C` is `so(A ?? B !! C)`';
is-deeply (not 1 ?? 1 !! 0), False, '`not A ?? B !! C` is `not(A ?? B !! C)`';

my $x = so 1 ?? 5 !! 0;
is-deeply $x, True, 'the conditional under `so` on an assignment RHS';

is-deeply (0 || so 0 ?? 5 !! 6), True, '`so` after `||` still takes the whole conditional';

my $y = not $x = 0;
is-deeply $y, True, '`not $x = 0` is `not($x = 0)`';
is-deeply $x, 0, 'the assignment under `not` happened';

is-deeply (so 1, 2), (True, 2), '`so` is tighter than the comma';
is-deeply (not 0 and 1), 1, '`not` is tighter than `and`';

my @a = (so 1 ?? 0 !! 1), 3;
is-deeply @a, [False, 3], 'parenthesized `so` conditional as a list element';
