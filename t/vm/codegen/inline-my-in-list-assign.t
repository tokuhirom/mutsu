use v6;
use Test;

plan 13;

# An inline `my` declaration inside a parenthesized list-assignment target
# (`(my $x, my $y) = 1, 2`) declares the variables and assigns to them, exactly
# like the more common `my ($x, $y) = 1, 2`. mutsu used to route the paren list
# through the callable-lvalue path and throw "Cannot modify an immutable value".
# (Regression driver: Math::Trig's `(my $x, my $y, $z) = cylindrical-to-cartesian(...)`.)

{
    (my $x, my $y) = 1, 2;
    is $x, 1, 'inline my: first scalar';
    is $y, 2, 'inline my: second scalar';
}

# Mixed: inline `my` declarations alongside a pre-declared variable.
{
    my $z;
    (my $x, my $y, $z) = 10, 20, 30;
    is $x, 10, 'mixed: inline first';
    is $y, 20, 'mixed: inline second';
    is $z, 30, 'mixed: pre-declared third';
}

# A single inline my in parens.
{
    (my $only) = 42;
    is $only, 42, 'single inline my in parens';
}

# Inline my for an aggregate target: the first `@` is greedy and slurps the
# remaining RHS values (`(my @a, my $x) = (1,2), 3` → @a = [(1,2), 3], $x = Any).
{
    (my @a, my $x) = (1, 2), 3;
    is @a.raku, '[(1, 2), 3]', 'inline my @a slurps the remaining RHS';
    ok !$x.defined, 'trailing scalar after a slurpy @ is undefined';
}

# Inline my for a hash target.
{
    (my $first, my %rest) = 'k', a => 1, b => 2;
    is $first, 'k', 'scalar before slurpy hash';
    is %rest.keys.sort.join(' '), 'a b', 'inline my %h slurps remaining pairs';
}

# The result of the list assignment (in list context) is the assigned targets.
{
    my @r = ((my $a, my $b) = 5, 6);
    is @r.elems, 2, 'list-assign result has one item per target';
    is @r.join(' '), '5 6', 'list-assign result values';
}

# Re-running the same declaration form does not leak the previous values.
{
    (my $p, my $q) = 7, 8;
    is "$p $q", '7 8', 'inline my values are the assigned ones, not Nil';
}
