use Test;

plan 9;

# `:x(Range)` in the operator substitution forms (S/// and s///): replace at
# least `lo` and at most `hi` matches, and set `$/` to a List of Match objects.

# Non-destructive S///
is-deeply (S:x(1..3)/./Z/ with 'abcd'), 'ZZZd', 'S:x(1..3) replaces up to 3';
is-deeply $/».Str, ('a', 'b', 'c'), 'S:x(1..3) $/ is the matched List';
isa-ok $/, List, 'S:x(range) sets $/ to a List';

# Destructive s///
{
    my $v = 'abcd';
    $v ~~ s:x(1..3)/./Z/;
    is $v, 'ZZZd', 's:x(1..3) replaces up to 3 in place';
    is-deeply $/».Str, ('a', 'b', 'c'), 's:x(1..3) $/ matched List';
}

# Range upper bound caps the replacements even with more matches available.
{
    my $v = 'aaaaa';
    $v ~~ s:x(1..2)/a/X/;
    is $v, 'XXaaa', 's:x(1..2) caps at 2';
}

# Too few matches for the lower bound: no substitution.
{
    my $v = 'ab';
    $v ~~ s:x(3..5)/./Z/;
    is $v, 'ab', 's:x(3..5) with too few matches is a no-op';
}

# Exact count still works (regression).
{
    my $v = 'aaaa';
    $v ~~ s:x(2)/a/b/;
    is $v, 'bbaa', 's:x(2) exact count still works';
}

# `:Nx` shorthand still works (regression).
{
    my $v = 'aaaa';
    $v ~~ s:2x/a/b/;
    is $v, 'bbaa', 's:2x shorthand still works';
}
