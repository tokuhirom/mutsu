use Test;

plan 6;

# A trailing comma before a terminator belongs to the call's argument list,
# so a single-argument builtin call is not wrapped in a one-element List.
sub f { flip "ab", ; }
is f().raku, '"ba"', 'flip with trailing comma';
sub g { lc "AB", ; }
is g().raku, '"ab"', 'lc with trailing comma';
is (sprintf "%s", "ab",).raku, '"ab"', 'sprintf with trailing comma in parens';
my $x = (uc "ab",);
is $x.raku, '"AB"', 'uc with trailing comma in parens';
sub h { lc "AB",
}
is h().raku, '"ab"', 'trailing comma before closing brace';
is (join "-", "a", "b",).raku, '"a-b"', 'join with trailing comma';
