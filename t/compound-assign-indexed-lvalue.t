use v6;
use Test;

# Compound assignment on an indexed lvalue (`@a[i] OP= v`, `%h<k> OP= v`).
# The RHS operator parsers must leave every `OP=` form for the assignment
# parser; otherwise the operator parser eats the base op and strands the `=`,
# so `@a[0] /= 2` (and many bitwise/shift/power/replication forms) failed to
# parse while `@a[0] *= 2` / `+= 2` worked. Regression from Math::Matrix, which
# uses `@U[$i][$j] /= $f`.

plan 29;

# Array element lvalues.
{
    my @a = 8, 4;
    @a[0] /= 2;   is @a[0], 4,  '/= on array element';
    @a[0] **= 2;  is @a[0], 16, '**= on array element';
    @a[1] += 6;   is @a[1], 10, '+= on array element (regression guard)';
    @a[1] *= 2;   is @a[1], 20, '*= on array element (regression guard)';
}
{
    my @b = 6, 5;
    @b[0] +&= 3;  is @b[0], 2,  '+&= (numeric bit-and) on array element';
    @b[1] +|= 2;  is @b[1], 7,  '+|= (numeric bit-or) on array element';
    @b[0] +^= 1;  is @b[0], 3,  '+^= (numeric bit-xor) on array element';
    @b[1] +<= 1;  is @b[1], 14, '+<= (bit shift left) on array element';
    @b[1] +>= 2;  is @b[1], 3,  '+>= (bit shift right) on array element';
}
{
    my @c = True, False;
    @c[0] ?&= False; is @c[0], False, '?&= (bool bit-and) on array element';
    @c[1] ?|= True;  is @c[1], True,  '?|= (bool bit-or) on array element';
    @c[0] ?^= True;  is @c[0], True,  '?^= (bool bit-xor) on array element';
}
{
    # Equal-length operands avoid a pre-existing unequal-length `~&` padding
    # quirk (orthogonal to this parse fix); the point here is that `~&=` on an
    # indexed lvalue *parses* and applies the string bitwise-and.
    my @d = "abd", "abc";
    @d[0] ~&= "abc"; is @d[0], "ab`", '~&= (string bit-and) on array element';
}
{
    my @e = 3, 3;
    @e[0] x= 3;   is @e[0], "333",  'x= (string repeat) on array element';
    my @f = 3, 3;
    @f[0] xx= 2;  is @f[0].elems, 2, 'xx= (list repeat) on array element (count)';
    is @f[0][0], 3, 'xx= produced the repeated element';
}

# Hash value lvalues (associative subscript).
{
    my %h = a => 8, b => 6;
    %h<a> /= 2;   is %h<a>, 4,  '/= on hash value';
    %h<b> **= 2;  is %h<b>, 36, '**= on hash value';
    %h<a> +<= 3;  is %h<a>, 32, '+<= on hash value';
}
{
    my %g = x => 6;
    %g{'x'} +&= 3; is %g{'x'}, 2, '+&= on hash value via {} subscript';
}

# Nested index lvalues (the exact Math::Matrix shape `@U[$i][$j] /= $f`).
{
    my @m = [8, 4], [2, 16];
    @m[0][0] /= 2;  is @m[0][0], 4, '/= on nested array element';
    @m[1][1] **= 2; is @m[1][1], 256, '**= on nested array element';
}

# Plain-operator regression guards (the guards must not change these).
is (10 / 3).round(0.0001), 3.3333, 'plain / still works';
is 2 ** 10, 1024,   'plain ** still works';
is ("a" x 3), "aaa", 'plain x still works';
is (6 +& 3), 2,      'plain +& still works';
is (6 +| 1), 7,      'plain +| still works';
is (1 +< 4), 16,     'plain +< still works';
is (10 %% 5), True,  'plain %% still works';
