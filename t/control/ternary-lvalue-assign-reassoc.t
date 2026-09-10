use v6;
use Test;

# Regression: `cond ?? $a !! $b = rhs` must parse as `(cond ?? $a !! $b) = rhs`
# because the assignment operator `=` is LOOSER than the conditional `?? !!`.
# mutsu previously rejected it ("Assignment operators inside ?? !! are too
# loose; parenthesize them") -- the else-branch greedily absorbed the
# assignment instead of leaving it to bind around the whole ternary.
# Surfaced loading Zef::Distribution:
#   $!ver ~~ Version ?? $_ !! $!ver = Version.new($_ // 0)

plan 7;

{
    my $a; my $b;
    1 ?? $a !! $b = 5;
    is $a, 5, 'true cond: lvalue ternary assigns to then-branch target';
    ok !$b.defined, 'else-branch target untouched when cond true';
}

{
    my $a; my $b;
    0 ?? $a !! $b = 5;
    is $b, 5, 'false cond: lvalue ternary assigns to else-branch target';
    ok !$a.defined, 'then-branch target untouched when cond false';
}

# A `~~`-condition (Binary cond) is the exact Zef::Distribution shape.
{
    my $ver;
    my $version = '1.2';
    my $r = do {
        with $ver // $version {
            $ver ~~ Version ?? $_ !! $ver = Version.new($_ // 0)
        }
    };
    is $r, v1.2, 'Zef::Distribution ver() pattern: smartmatch cond + else-assign';
    is $ver, v1.2, 'else-branch attribute-like var was assigned the new Version';
}

# Parenthesized form keeps working (explicit lvalue ternary).
{
    my $a; my $b;
    ($a ?? $a !! $b) = 7;
    is $b, 7, 'explicit parenthesized lvalue ternary still works';
}
