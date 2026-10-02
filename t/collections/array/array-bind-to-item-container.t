use Test;

# `my @b := @a[0]` / `my @b := $s` bind `@b` to the Array held in the Scalar
# container, not to the container itself: replacing the container's content
# later leaves `@b` alone, and an `@`/`%` variable is never `=:=` to a Scalar
# container unless it was `:=`-bound to it. Every expectation below was checked
# against rakudo (#10692).

plan 14;

{
    my @a = [1, 2],;
    my @b := @a[0];
    nok @a[0] =:= @b, '@a[0] =:= @b is False after @b := @a[0]';
    nok @b =:= @a[0], '... and with the operands swapped';
    @b.push(3);
    is-deeply @a, [[1, 2, 3],], 'the bound Array is shared';
    @a[0] = [9];
    is-deeply @b, [1, 2, 3], 'replacing the element leaves @b bound to the old Array';
}

{
    my @w = [1, 2],;
    my @v := @w[*-1];
    @w[0] = [8];
    is-deeply @v, [1, 2], 'a *-1 index binds the element value too';
}

{
    my $s = [1];
    my @d := $s;
    nok $s =:= @d, '$s =:= @d is False after @d := $s';
    $s = [5];
    is-deeply @d, [1], 'reassigning $s leaves @d bound to the old Array';
    my $t = [1];
    my @f := $t;
    @f.push(2);
    is-deeply $t, [1, 2], '... while the Array itself is shared';
}

{
    my @c = 1;
    my @d;
    @d[0] := @c;
    ok @d[0] =:= @c, 'an element bound to an Array is =:= to it';
}

{
    my @y;
    my $x := @y;
    ok $x =:= @y, 'a $ bound to an Array is =:= to it';
    my @z;
    my $w = @z;
    nok $w =:= @z, 'a $ assigned an Array owns its own Scalar';
    my %g;
    my $j = %g;
    nok %g =:= $j, '... for a Hash too';
    my %h;
    my $k := %h;
    ok %h =:= $k, 'a $ bound to a Hash is =:= to it';
}

{
    my @s = 1, 2, 3;
    my @sl := @s[1, 2];
    @sl[0] = 9;
    is-deeply @s, [1, 9, 3], 'a slice bind still aliases the elements';
}
