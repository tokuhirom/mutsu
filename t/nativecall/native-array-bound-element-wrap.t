use v6;
use Test;
use nqp;

# #11233: a write through a bound element of a native array is coerced
# exactly like a direct `@a[i] = v` store. Every way of writing an element
# agrees: direct store, `:=` alias, `nqp::atposref_*`, lvalue return.

plan 14;

{
    my uint8 @u = 1, 2;
    @u[1] = 300;
    my $q := @u[0];
    $q = 257;
    is-deeply @u.List, (1, 44), 'uint8: a bound element wraps like a direct store';
    is ($q = 300), 44, 'the assignment through the alias yields the stored value';
}

{
    my int8 @a = 0;
    my $r := @a[0];
    $r = 200;
    is @a[0], -56, 'int8: a bound element wraps to the signed range';
}

{
    my uint8 @u = 1, 2, 3;
    my $r := nqp::atposref_u(@u, 2);
    $r = 513;
    is-deeply @u.List, (1, 2, 1), 'nqp::atposref_u hands back a wrapping element';
}

{
    sub first-elem(\a) is rw { a[0] }
    my int8 @s = 0;
    first-elem(@s) = 200;
    is @s[0], -56, 'an lvalue return of a native element wraps';
}

{
    my uint8 @v = 0;
    my $z := @v[0];
    $z = 255;
    $z++;
    is @v[0], 0, '++ through a bound uint8 element wraps';
    $z += 300;
    is @v[0], 44, '+= through a bound uint8 element wraps';
    my uint16 @w = 0;
    my $c := @w[0];
    $c -= 1;
    is @w[0], 65535, '-= through a bound uint16 element wraps below zero';
}

{
    my int @b = 0;
    my $r := @b[0];
    throws-like { $r = 2**70 }, Exception,
        message => /'Cannot unbox 71 bit wide bigint'/,
        'a full-width int element refuses an oversized bigint';
    is @b[0], 0, 'the refused store leaves the element unchanged';
}

{
    my num32 @n = 0e0;
    my $s := @n[0];
    $s = 0.1e0;
    my num32 $single = 0.1e0;
    is @n[0], $single, 'num32: a bound element rounds to single precision';
}

# Growing past the end through a bound element fills the gap with the
# element type's zero, not its type object.
{
    my int16 @h = 0;
    my $d := @h[3];
    $d = 40000;
    is-deeply @h.List, (0, 0, 0, -25536), 'int16: holes are 0 and the write wraps';
    my str @s;
    my $t := @s[1];
    $t = "a";
    is @s.raku, 'array[str].new("", "a")', 'str: holes are the empty string';
}

{
    my Int @i = 0;
    my $e := @i[0];
    $e = 2**70;
    is @i[0], 2**70, 'a boxed Int element stores a bigint untouched';
}
