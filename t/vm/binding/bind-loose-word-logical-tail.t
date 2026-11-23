use Test;

# #18938: `:=` is item-assignment precedence, so a loose word-logical
# (`and`, `or`, `andthen`, `orelse`, ...) ends the bound value and applies to
# the bind: `my $x := 1 orelse 2` is `(my $x := 1) orelse 2`.

plan 10;

{
    my $x := Nil orelse 2;
    is $x.raku, 'Nil', 'declaration: orelse is looser than :=';
}
{
    my $y := 5 andthen 7;
    is $y, 5, 'declaration: andthen is looser than :=';
}
{
    my $ran = 0;
    my $x := 0 or $ran = 1;
    is $x, 0, 'declaration: or does not reach the bound value';
    is $ran, 1, 'declaration: the or tail runs on a false bind';
}
{
    my @a := [1, 2] and my $t = 'tail';
    is @a, [1, 2], 'array declaration binds before and';
    is $t, 'tail', 'its and tail runs';
}
{
    my $z;
    $z := 5 andthen 7;
    is $z, 5, 'plain bind: andthen is looser than :=';
}
{
    my $z;
    my $ran = 0;
    $z := Nil orelse $ran = 1;
    is $z.raku, 'Nil', 'plain bind: orelse does not reach the bound value';
    is $ran, 1, 'plain bind: the orelse tail runs';
}

is (my $q := 1 orelse 2), 1, 'a parenthesized bind declaration';
