use Test;

# `do STMT` in expression context wraps a single statement. When that statement
# carries a statement modifier (`do $_ for @list`) or is an assignment, the inner
# statement parser used to consume the trailing `;`, so a following listop was
# misparsed as an infix operator on the `do` result
# (`my @a = do $_ for 2..4; say @a` became `my @a = say(do(...), @a)`).
# The `;` belongs to the outer statement and must terminate the `do` expression.

plan 9;

# do EXPR for LIST collects each iteration's value as an rvalue
my @a = do $_ for 2..4;
is-deeply @a, [2, 3, 4], 'do $_ for <int range> collects values';

my @b = do $_ * 2 for 2..4;
is-deeply @b, [4, 6, 8], 'do EXPR for <range> collects computed values';

# the range-arithmetic interaction from S03-operators/range.t
my @c = do $_ for (2..4) + 5e-1;
is-deeply @c, [2.5e0, 3.5e0, 4.5e0], 'do $_ for <Num range> collects per-iteration values';

# a following listop must NOT be absorbed as an infix operator on the do result
my @d = do $_ for 2..4;
say 99 for ();  # no-op; just ensure the previous statement terminated
is-deeply @d, [2, 3, 4], 'do-for terminates before a following statement';

# do for as a scalar rvalue holds the collected List
my $last = do $_ for 1..3;
is $last.gist, '(1 2 3)', 'do $_ for as scalar rvalue is the collected List';

# do EXPR if COND
my $x = do 42 if True;
is $x, 42, 'do EXPR if True returns the value';

my $y = do 42 if False;
ok !$y.defined, 'do EXPR if False is undefined';

# do ASSIGN; following statement stays separate
my $p;
my $q = do $p = 7;
is $q, 7, 'do ASSIGN returns the assigned value';
is $p, 7, 'do ASSIGN performed the assignment';
