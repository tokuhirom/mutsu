use v6;
use Test;

# A word-operator compound assignment (`div=`, `mod=`, `gcd=`, `lcm=`) must
# parse as the operand of a looser operator such as `and`/`or`, e.g.
# `@f.push($p) and $n div= $p while ...`. Previously the multiplicative /
# custom-infix parsers ate the bare `div` (leaving a stray `= 2`), so the whole
# expression failed to parse. Surfaced by Prime::Factor.

plan 8;

{
    my $n = 12;
    1 and $n div= 2;
    is $n, 6, 'div= as the RHS of `and`';
}
{
    my $n = 17;
    1 and $n mod= 5;
    is $n, 2, 'mod= as the RHS of `and`';
}
{
    my $n = 12;
    0 or $n gcd= 8;
    is $n, 4, 'gcd= as the RHS of `or`';
}
{
    my $n = 4;
    1 and $n lcm= 6;
    is $n, 12, 'lcm= as the RHS of `and`';
}
{
    # The exact Prime::Factor shape: a statement-modifier `while` around an
    # `and` whose RHS is a word compound assignment.
    my @factors;
    my $n = 12;
    @factors.push(2) and $n div= 2 while $n %% 2;
    is @factors.join(','), '2,2', 'push and div= while — factors collected';
    is $n, 3, 'push and div= while — n reduced';
}

# Regressions: the word operators still parse as plain infixes.
is (17 div 5), 3, 'div infix still works';
is (12 gcd 8), 4, 'gcd infix still works';
