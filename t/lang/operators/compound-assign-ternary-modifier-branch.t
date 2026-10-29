use v6;
use Test;

plan 5;

# `COND ?? (S if C) !! $x ~= V` parses as `(COND ?? (S if C) !! $x) ~= V`: the
# compound assignment goes to the selected branch, and an unrun modifier
# yields Empty, which is a silent no-op (P5reset's pattern expansion).
{
    my $start = "a";
    my $letters;
    for "-", "c" -> $letter {
        $letter eq '-' ?? ($start = "z" if $letters) !! $letters ~= "X";
    }
    is $letters, "X", 'else branch assigned, unrun modifier branch ignored';
    is $start, "a", 'the modifier body did not run';
}

{
    my $start = "a";
    my $letters = "q";
    for "-" -> $letter {
        $letter eq '-' ?? ($start = "z" if $letters) !! $letters ~= "X";
    }
    is $start, "zX", 'a run modifier body takes the compound assignment';
}

{
    my $s = "s";
    my $n = 0;
    1 ?? ($s = "t" if 0) !! $n += 1;
    is $s, "s", 'false modifier leaves the target alone';
    is $n, 0, 'and the other branch is not taken';
}
