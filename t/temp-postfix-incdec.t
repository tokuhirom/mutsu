use v6;
use Test;

# `(temp $x)++` / `(temp $x)--` (parenthesized temp used as an lvalue) yields
# the live container, so the postfix op mutates it and returns the old value —
# the same lvalue behavior as the prefix `++temp $x` form.
{
    my $x = 5;
    my $old = (temp $x)++;
    is $old, 5, '(temp $x)++ returns the old value';
    is $x, 6, '(temp $x)++ increments the live variable';
}
{
    my $y = 5;
    my $old = (temp $y)--;
    is $old, 5, '(temp $y)-- returns the old value';
    is $y, 4, '(temp $y)-- decrements the live variable';
}

# temp inside a sub restores at sub exit, so an entangled counter reflects the
# call depth: the argument calls run at depth 1, then the outer call is also
# depth 1 after they restore (raku-doc Language/variables `temp` example).
my @depths;
my $in = 0;
sub depth(*@c) {
    (temp $in)++;
    @depths.push: $in;
    $in;
}
depth(depth(), depth());
is @depths, [1, 1, 1], 'temp restores at each sub exit (every call sees depth 1)';
is $in, 0, 'temp fully restored after the outer call';

done-testing;
