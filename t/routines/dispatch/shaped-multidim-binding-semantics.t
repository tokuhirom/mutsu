use Test;
plan 36;

{
    my @s[2;2];
    my $x = 1;
    @s[0;0] := $x;
    $x = 5;
    is @s[0;0], 1, 'shaped semicolon bind takes a value snapshot';
    dies-ok { @s[0;0] = 7 }, 'the shaped value-bound slot is readonly';
    is $x, 5, 'a rejected shaped store leaves the source alone';
    my $y = 2;
    lives-ok { @s[0;0] := $y }, 'a readonly shaped slot can be rebound';
    $y = 6;
    is @s[0;0], 2, 'a shaped rebind takes a new value snapshot';
}
{
    my @n = [1,2], [3,4];
    my $x = 9;
    @n[0;1] := $x;
    $x = 10;
    is @n[0;1], 9, 'nested semicolon bind takes a value snapshot';
    dies-ok { @n[0;1] = 11 }, 'the nested value-bound slot is readonly';
    is $x, 10, 'a rejected nested store leaves the source alone';
    my $y = 12;
    lives-ok { @n[0;1] := $y }, 'a readonly nested slot can be rebound';
    $y = 13;
    is @n[0;1], 12, 'a nested rebind replaces the old value';
}
{
    my @s[3];
    my $q = 1;
    @s[1] := $q;
    $q = 3;
    is @s[1], 3, 'one-dimensional shaped bind shares its source';
    @s[1] = 4;
    is $q, 4, 'a shaped element writes through its first binding';
    my $r = 2;
    lives-ok { @s[1] := $r }, 'a shaped element accepts a different container';
    $r = 7;
    is @s[1], 7, 'a shaped rebind reads its new source';
    @s[1] = 8;
    is $r, 8, 'a shaped rebind writes through its new source';
    is $q, 4, 'the old source is unaffected by the rebind';
    $q = 99;
    is @s[1], 8, 'the old source no longer controls the shaped slot';
}
{
    my @s[2];
    @s[0] := 1;
    dies-ok { @s[0] = 2 }, 'a literal-bound shaped slot is readonly';
    my $x = 3;
    lives-ok { @s[0] := $x }, 'a literal-bound shaped slot can bind a container';
    @s[0] = 4;
    is $x, 4, 'the new writable binding replaces the readonly one';
}
{
    my @s[2;2;2];
    my $x = 5;
    @s[1;0;1] := $x;
    $x = 6;
    is @s[1;0;1], 5, 'three shaped dimensions bind a value';
    dies-ok { @s[1;0;1] = 7 }, 'a three-dimensional shaped bind is readonly';
    @s[1;0;1] := 8;
    is @s[1;0;1], 8, 'three shaped dimensions can be rebound';
}
{
    my @n = [[1,2], [3,4]], [[5,6], [7,8]];
    my $x = 10;
    @n[1;0;1] := $x;
    $x = 11;
    is @n[1;0;1], 10, 'three nested dimensions bind a value';
    dies-ok { @n[1;0;1] = 12 }, 'a three-dimensional nested bind is readonly';
    @n[1;0;1] := 13;
    is @n[1;0;1], 13, 'three nested dimensions can be rebound';
}
{
    my @n = [1,2], [3,4];
    my $x = 5;
    @n[0][1] := $x;
    $x = 6;
    is @n[0][1], 6, 'chained brackets still bind the source container';
    @n[0][1] = 7;
    is $x, 7, 'chained brackets still write through the source';
    lives-ok { @n[0][1] := 8 }, 'a chained element can be rebound to a literal';
    dies-ok { @n[0][1] = 9 }, 'a literal rebind replaces a chained alias';
}
{
    my @s[2;2];
    my $x = 1;
    (@s[0;1] := $x);
    $x = 2;
    is @s[0;1], 1, 'expression-context semicolon bind takes a snapshot';
    dies-ok { @s[0;1] = 3 }, 'expression-context semicolon bind is readonly';
}
{
    my @s[2;2];
    my $x = 1;
    my &change = { $x = 9 };
    @s[0;1] := $x;
    change();
    is @s[0;1], 1, 'an already captured source cell is decontainerized';
    my @one[2];
    @one[0] := $x;
    @one[0] = 10;
    is $x, 10, 'one-dimensional binding still shares a captured source cell';
    my @alias := @one;
    my $y = 11;
    @alias[0] := $y;
    is @one[0], 11, 'rebinding through a shaped array alias updates the shared slot';
    @one[0] = 12;
    is $y, 12, 'the alias-installed binding writes through the new source';
}
