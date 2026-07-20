use Test;

# A parenthesized `my`/`our`/`state` destructuring declaration used as an
# `if`/`unless`/`elsif` condition. The declared targets must be visible in the
# conditional body (Raku scopes the condition declaration to the whole `if`
# construct), and the trailing `{ ... }` block must be parsed as the body — NOT
# swallowed as a hash subscript of the declaration. Regression for the
# Audio::Liquidsoap parse failure (`if my ( $key, $value) = get-metadata-pair(...)
# { ... }`).

plan 11;

# Basic destructure-in-if: targets visible in the body.
{
    my $out;
    if my ($a, $b) = (1, 2) {
        $out = "$a $b";
    }
    is $out, '1 2', 'if my ($a, $b) = (...) { } sees $a and $b in the body';
}

# Single-target parenthesized destructure.
{
    my $out;
    if my ($x) = 42 {
        $out = $x;
    }
    is $out, 42, 'if my ($x) = v { } sees $x in the body';
}

# The condition is truthy when the RHS has values.
{
    my $ran = False;
    if my ($p, $q) = (10, 20) { $ran = True }
    ok $ran, 'destructure condition with values takes the then-branch';
    is $p, 10, 'target $p leaks to the enclosing scope';
    is $q, 20, 'target $q leaks to the enclosing scope';
}

# `unless` with a destructure condition.
{
    my $ran = False;
    unless my ($a, $b) = () { $ran = True }
    # empty RHS: no values assigned; the else-less unless still declares $a/$b
    ok $a.^name eq 'Any' || $a.defined.not,
        'unless my ($a, $b) = () declares the targets';
}

# `elsif` with a destructure condition.
{
    my $out = 'none';
    if my ($z) = () and $z.defined {
        $out = "z=$z";
    } elsif my ($w) = (99) {
        $out = "w=$w";
    }
    is $out, 'w=99', 'elsif my ($w) = (...) binds and takes its branch';
}

# `while` with a destructure condition (uses the same condition parser family).
{
    my @q = (1, 2, 3);
    my @seen;
    while my ($item) = @q.shift {
        last unless $item.defined;
        @seen.push($item);
    }
    is @seen.join(','), '1,2,3', 'while my ($item) = @q.shift { } iterates';
}

# Expression-context destructure leaks its targets (not just conditions).
{
    (my ($m, $n) = (7, 8));
    is "$m $n", '7 8', '(my ($m, $n) = (...)) leaks $m and $n';
}

# Destructure body still parses correctly with a multi-statement block.
{
    my $sum = 0;
    if my ($a, $b, $c) = (1, 2, 3) {
        $sum += $a;
        $sum += $b;
        $sum += $c;
    }
    is $sum, 6, 'multi-statement then-branch after a destructure condition';
}

# Greedy trailing array target in the destructure condition.
{
    my $out;
    if my ($head, @rest) = (1, 2, 3, 4) {
        $out = "$head|{@rest.join(',')}";
    }
    is $out, '1|2,3,4', 'greedy @rest target works in a destructure condition';
}
