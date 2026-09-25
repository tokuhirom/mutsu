use Test;

# `temp @a` / `temp %h` detaches the container's bound elements for the scope
# and reattaches them at scope exit, as rakudo does: inside the scope a write to
# a bound element does not reach the variable it was bound to, and afterwards
# the element is bound again (#9435).

plan 12;

{
    my $x = 1;
    my @c;
    @c[0] := $x;
    { temp @c; @c[0] = 5; is $x, 1, 'inside: an element write does not reach the bound variable' }
    is $x, 1, 'after: the bound variable is unchanged';
    is @c[0], 1, 'after: the element is restored';
}

{
    my $y = 1;
    my @d;
    @d[0] := $y;
    { temp @d; $y = 7; is @d[0], 1, 'inside: a write to the bound variable does not reach the element' }
    is @d[0], 7, 'after: the element is bound to the variable again';
    $y = 9;
    is @d[0], 9, 'after: the binding is live';
}

{
    my $z = 1;
    my %h;
    %h<a> := $z;
    { temp %h; %h<a> = 5; is $z, 1, 'hash, inside: a write does not reach the bound variable' }
    $z = 3;
    is %h<a>, 3, 'hash, after: the binding is live again';
}

{
    my $w = 1;
    my @e;
    @e[0] := $w;
    my @alias := @e;
    { temp @e; @e[0] = 3; is "{@alias} $w", '3 1', 'an alias of the container sees the detached element' }
    is "{@alias} $w", '1 1', 'the alias sees the restore';
}

{
    my @f = 1, 2;
    my $r = @f;
    { temp @f; @f[0] = 9; is $r, '9 2', 'a reference to the container sees the write' }
    ok $r === @f, 'the container keeps its identity across the restore';
}
