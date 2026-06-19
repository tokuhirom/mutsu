use Test;

plan 8;

# A greedy `*`/`+` over a variable-length alternation must be able to backtrack
# into a *shorter* per-iteration choice to satisfy a later constraint. Verified
# against Rakudo.

# LTM alternation backtracking to satisfy a trailing word boundary.
{
    my $m = "abcde" ~~ / (a | b | bc | cde)+»/;
    ok $m, 'LTM alternation + » matches';
    is $m[0].elems, 3, 'three captures (a, b, cde)';
    is join(" ", $m[0]), 'a b cde', 'correct captures after backtracking';
}

# Without the anchor, greedy per-step LTM wins (a, bc) — no backtracking.
{
    my $m = "abcde" ~~ / (a | b | bc | cde)+/;
    is join(" ", $m[0]), 'a bc', 'greedy per-step LTM without anchor';
}

# Anchored alternation `+` must consume the whole string.
{
    my $m = "abab" ~~ / (a | b)+ $/;
    is $m[0].elems, 4, 'anchored (a|b)+ consumes all';
}

# Backtrack into a shorter alternative to let a following literal match.
{
    ok "aXb" ~~ / (a | aX)+ b /, '(a|aX)+ b backtracks aX->a? no: matches';
    my $m = "aab" ~~ / (a | ab)+ b $/;
    is join(" ", $m[0]), 'a a', '(a|ab)+ b backtracks last ab->a';
}

# A plain (non-alternation) quantifier is unaffected.
{
    "aaab" ~~ / (a)+ b $/;
    is $/[0].elems, 3, 'plain (a)+ still greedy';
}
