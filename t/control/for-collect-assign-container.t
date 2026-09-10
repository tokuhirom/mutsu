use Test;

plan 5;

# A value-collecting `for` loop whose body's last statement is a bare
# assignment must collect the assigned *container*, not Nil. Raku assignment
# is an expression returning the lvalue container, so collecting `$s += $_`
# each iteration yields the same `$s` container (all read as the final value).
# https://github.com/Raku/old-issue-tracker/issues/2080 (S04-statements/for.t)
{
    my $s = 0;
    is-deeply (for 1..3 { $s += $_ }), (6, 6, 6),
        'for loop collects the assigned container (does not decontainerize)';
}

{
    sub foo { my $s; (for 1..3 { $s += $_ }) }
    is-deeply foo(), (6, 6, 6), 'for loops do not decontainerize (in a sub)';
}

# Plain `=` assignment in the body behaves the same way.
{
    my $x = 0;
    is-deeply (for 1..3 { $x = $_ }), (3, 3, 3),
        'plain = assignment in for body collects the container';
}

# The parenthesized form already worked; confirm parity.
{
    my $y = 0;
    is-deeply (for 1..3 { ($y += $_) }), (6, 6, 6),
        'parenthesized assignment matches the bare form';
}

# A non-assignment last statement still collects per-iteration values.
{
    is-deeply (for 1..3 -> $v { $v * 2 }), (2, 4, 6),
        'non-assignment body still collects per-iteration values';
}
