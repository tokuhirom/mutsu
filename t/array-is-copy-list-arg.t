use v6;
use Test;

# An `@`-sigil `is copy` parameter is a *mutable Array* copy of its argument,
# even when the argument is an (immutable) List (`<x y>`, `(1,2)`). Element
# assignment, `.push`, and `(@a[i],@a[j]).=reverse` must all work inside the
# sub; without the fix they threw "Cannot modify an immutable List". The copy
# is distinct, so the caller's data is never mutated. Regression pin for SortUk.

plan 10;

{
    sub f(@d is copy) { @d[0] = 99; @d }
    is-deeply f(<x y>), [99, 'y'], 'element assign on is-copy of a <> List';
    is-deeply f((1, 2)), [99, 2],  'element assign on is-copy of a () List';
}

{
    sub f(@d is copy) { @d.push(9); @d }
    is-deeply f(<x y>), ['x', 'y', 9], 'push on is-copy of a List (result is an Array)';
}

{
    # The gitignore-swap shape used by SortUk's sort loop.
    sub swap(@d is copy) { (@d[0], @d[1]).=reverse; @d }
    is-deeply swap(<a b>), ['b', 'a'], '(@d[0],@d[1]).=reverse swaps elements';
}

{
    # A real-Array argument is copied, not aliased: the caller is untouched.
    sub f(@d is copy) { @d[0] = 99; @d }
    my @a = 1, 2;
    is-deeply f(@a), [99, 2], 'is-copy of an Array mutates the copy';
    is-deeply @a, [1, 2],     'the caller Array is unchanged (distinct copy)';
}

{
    # A List argument is likewise copied; mutating the param keeps its size.
    sub f(@d is copy) { @d[0] = 'Z'; @d.elems }
    is f(<a b c>), 3, 'is-copy List keeps its elements';
}

{
    # Repeated push (statement-modifier `for`) on the copy accumulates.
    sub build(@d is copy) {
        @d.push($_) for 10, 20, 30;
        @d
    }
    is-deeply build(<s t>), ['s', 't', 10, 20, 30], 'repeated push on is-copy List';
}

{
    # Nested: an inner element read after a swap sees the swapped value.
    sub f(@d is copy) {
        (@d[0], @d[2]).=reverse;
        @d[0]
    }
    is f(<p q r>), 'r', 'read after in-place element swap on is-copy List';
}

{
    # `is copy` of an empty List still yields a mutable Array.
    sub f(@d is copy) { @d.push(1); @d }
    is-deeply f(()), [1], 'push onto is-copy of an empty List';
}
