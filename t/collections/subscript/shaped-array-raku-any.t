use v6;
use Test;

# A real array's elements are Scalar containers, so a cell can never hold Nil:
# a freshly-declared shaped array (`my @a[2,2]`) defaults its cells to the
# element type (`Any` for an untyped array), and assigning Nil to a real-array
# element reverts it to that default. `.raku` must therefore render such cells
# as `Any`, not `Nil`. Lists (which are not `@`-sigiled) still keep a real Nil.

plan 6;

{
    my @a[2,2];
    is @a.raku, 'Array.new(:shape(2, 2), [Any, Any], [Any, Any])',
        '2D shaped array cells render as Any';
}

{
    my @b[3];
    is @b.raku, 'Array.new(:shape(3,), [Any, Any, Any])',
        '1D shaped array cells render as Any';
}

{
    my @c = 1, 2, 3;
    @c[1] = Nil;
    is @c.raku, '[1, Any, 3]', 'Nil assigned to a real-array element renders as Any';
}

# A typed shaped array keeps the element type object in its cells (not Any).
# (The `Array[Int]` vs `array[Int]` prefix casing is a separate known issue,
# so only the cell contents are asserted here.)
{
    my Int @d[3];
    ok @d.raku.contains('[Int, Int, Int]'),
        'typed shaped array cells render as the element type, not Any';
}

# A List keeps a genuine Nil (it is not an @-sigiled container).
is (1, Nil, 3).raku, '(1, Nil, 3)', 'a List preserves a real Nil element';

# Nil itself still renders as Nil.
is Nil.raku, 'Nil', 'bare Nil.raku is still Nil';
