use v6;
use Test;

# `push my @arr, VALUE` declares @arr in the current scope AS the first
# argument of the push listop. The array must actually receive the pushed
# elements (it used to fall through to the generic call and push into a
# throwaway copy, leaving @arr empty). Regression pin for Math::Angle.

plan 12;

{
    push my @u, 10;
    is-deeply @u, [10], 'push my @u, single value';
}

{
    push my @u, 10, 20, 30;
    is-deeply @u, [10, 20, 30], 'push my @u, multiple values';
}

{
    push (my @u), 5;
    is-deeply @u, [5], 'push (my @u), value';
}

{
    unshift my @u, 1, 2;
    is-deeply @u, [1, 2], 'unshift my @u';
}

{
    push my @u = (1, 2), 3;
    is-deeply @u, [(1, 2), 3], 'push my @u = (...), value keeps initializer';
}

{
    # The elements are readable/usable right after the inline-decl push.
    push my @u, 5 * 2;
    is @u[0].abs, 10, 'pushed element is a real value, not Any';
    is @u.elems, 1, 'elems reflects the push';
}

{
    # Plain (separately declared) form still works.
    my @u;
    push @u, 9;
    is-deeply @u, [9], 'separately declared push still works';
}

{
    # Inline decl inside a method body (the Math::Angle shape): the pushed
    # value comes from an attribute read.
    class C {
        has $!angle is built;
        method units() {
            push my @u, $!angle * 2;
            @u[0].abs
        }
    }
    is C.new(angle => 10).units, 20, 'push my @u of attribute inside method';
    is C.new(angle => -7).units, 14, 'push my @u of negative attribute';
}

{
    # Two independent inline-decl pushes in the same scope do not interfere.
    push my @a, 1;
    push my @b, 2;
    is-deeply @a, [1], 'first inline-decl array';
    is-deeply @b, [2], 'second inline-decl array';
}
