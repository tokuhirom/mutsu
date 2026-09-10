use Test;

plan 22;

# GitHub issue #6953: `$a += ($b = 2) / 2` failed to parse ("Regex not
# terminated") because the parenthesized-assignment shortcut inside
# `try_parse_assign_expr` recognized `($b = 2)` and stopped right after the
# closing `)`, leaving `/ 2` as a dangling leftover statement. In term
# position a leading `/` starts a regex literal, which never terminates.
# `+`/`-`/`*` didn't error, but silently produced the WRONG answer: the RHS
# collapsed to just `($b = 2)` and the trailing operator became a separate
# (sink-context) statement.
#
# `+=` is item assignment in Raku, which is LOOSER than the following tighter
# infix, so `$a += ($b = 2) / 2` must parse as `$a += (($b = 2) / 2)`.

# --- the exact minimal repros from the issue ---
{
    my $a; my $b = 1;
    $a += ($b = 2) / 2;
    is $a, 1, '$a += ($b = 2) / 2 -- division continues past the paren-assign';
    is $b, 2, '... and $b was still assigned by the inner paren-assignment';
}

{
    my $term = 0; my $sign = 1;
    $term += ($sign = -$sign) / 2;
    is $term, -0.5, '$term += ($sign = -$sign) / 2 -- Vacca-series minimal shape';
    is $sign, -1, '... and $sign was flipped by the inner paren-assignment';
}

# --- the silently-wrong-answer variants (+, -, *) ---
{
    my $a; my $b = 1;
    $a += ($b = 2) + 3;
    is $a, 5, '$a += ($b = 2) + 3 -- addition continues past the paren-assign';
    is $b, 2, '... and $b was still assigned';
}

{
    my $a; my $b = 1;
    $a += ($b = 2) * 3;
    is $a, 6, '$a += ($b = 2) * 3 -- multiplication continues past the paren-assign';
}

{
    my $a; my $b = 1;
    $a -= ($b = 2) - 3;
    is $a, 1, '$a -= ($b = 2) - 3 -- subtraction continues past the paren-assign';
    is $b, 2, '... and $b was still assigned';
}

# --- cases that must keep working (negative/regression coverage) ---
{
    my $a = 10; my $b = 1;
    $a = ($b = 2) / 2;
    is $a, 1, 'plain `=` (not compound) already worked and must keep working';
    is $b, 2, '... $b assigned';
}

{
    my $a; my $b = 1;
    $a += $b / 2;
    is $a, 0.5, 'no inner paren-assignment: += RHS is a plain division';
}

{
    my $a; my $b = 1;
    $a += (($b = 2) / 2);
    is $a, 1, 'extra parens make the division part of the grouped primary';
    is $b, 2, '... $b assigned';
}

{
    (my $) += ($ = 1) / 1;
    pass 'parenthesized anonymous-state lvalue does not blow up';
}

{
    (my $x) += ($ = 1) / 1;
    is $x, 1, 'parenthesized `my` lvalue target with paren-assign RHS';
}

{
    my $x; ($x) += ($ = 1) / 1;
    is $x, 1, 'parenthesized plain-var lvalue target with paren-assign RHS';
}

{
    my $a; my $b = 1;
    $a += (my $x = 1) / 2;
    is $a, 0.5, 'a `my` decl inside parens is not try_parse_assign_expr, falls through correctly';
    is $x, 1, '... and the `my` declaration itself still ran';
}

# --- the actual Vacca-series minimal shape (small loop) ---
{
    my ($power, $sign, $term) = 4, -1, 0;
    for $power..^2*$power { $term += ($sign = -$sign) / $_ }
    is $term, 0.07381, 'Vacca-series inner loop shape matches raku';
    is $sign, -1, '... sign ends flipped an odd number of times';
}

# --- the original motivating Rosetta Code program (small N, no .race for determinism) ---
sub vacca-gamma (\N where N > 1) {
    return (1/2 - 1/3) + [+] (2..N).map: -> \n {
        my ($power, $sign, $term) = 2**n, -1;
        for $power..^2*$power { $term += ($sign = -$sign) / $_ }
        n * $term
    }
}
is vacca-gamma(10), 0.574285301882304, 'Vacca series Euler-Mascheroni approximation (N=10)';
