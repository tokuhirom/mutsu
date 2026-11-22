use Test;

# #18807: `=:=` resolved `:=` aliases by name, so an alias `$b := $a` compared
# equal to a shadowing inner `my $a`, which is spelled the same but is another
# container. The slot-keyed bind pairs now tell the two apart.

plan 6;

{
    my $a = 1;
    my $b := $a;
    {
        my $a = 2;
        nok $b =:= $a, 'an alias of the outer $a is not the shadowing inner $a';
        $b = 9;
        is $a, 2, 'writing the alias leaves the inner $a alone';
    }
    is $a, 9, 'writing the alias reaches the outer $a';
    ok $b =:= $a, 'outside the shadow the alias is the outer $a';
}

{
    my $x = 1;
    my $y := $x;
    my $z := $y;
    ok $z =:= $x, 'a chain of aliases reaches its root';
    my $w = 1;
    nok $w =:= $y, 'an unrelated variable is not an alias';
}
