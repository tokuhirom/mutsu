use Test;

# §1.4 shadow-slot leaf: list assignment `($a, $b) = ...`, `(my $a) = ...`, and
# `(@a, %h)` targets must write the inner (live) shadow binding, not the outer
# (first) same-name slot. The list-assignment / parenthesized-declaration stores
# now prefer the compile-time-baked AssignExprLocal (emit_assign_local_or_name),
# like the general assignment path. Passes with the shadow-slot gate off (shared
# slot) AND on (distinct slots). Mirrors roast/S03-operators/assign.t.

plan 8;

# Scalar list-assign into a shadow.
my ($a, $b) = (1, 2);
{
    my ($a, $b);
    ($a, $b) = (7, 8);
    is $a, 7, 'inner shadow $a assigned via list-assign';
    is $b, 8, 'inner shadow $b assigned via list-assign';
}
is $a, 1, 'outer $a untouched';
is $b, 2, 'outer $b untouched';

# @/% targets in list-assign into shadows.
my @x = <a b>;
my %y = (k => 1);
{
    my @x;
    my %y;
    ($a, @x) = (0, 10, 20, 30);
    is @x.join(','), '10,20,30', 'inner shadow @x slurps in list-assign';
    ($b, %y) = (0, p => 5);
    is %y<p>, 5, 'inner shadow %y assigned in list-assign';
}
is @x.join(','), 'a,b', 'outer @x untouched';

# Parenthesized `my` list-assign (assign.t "Assignment into parentheses'd my").
{
    (my $c) = 1, 2, 3;
    is $c.elems, 3, 'parenthesized my list-assign captures the whole list';
}
