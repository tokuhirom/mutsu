use v6;
use Test;

# A real array's (`@`-sigiled) elements are Scalar containers, so a cell can
# never hold Nil: assigning Nil reverts it to the element type default (`Any`
# for an untyped array), and a shaped array fills unused cells the same way.
# `.gist`/`say` must render such a cell as the type object `(Any)`, not `Nil`.
# A List keeps a genuine Nil (it is not an @-sigiled container).

plan 7;

{
    my @a = 4, 8, 15, 16;
    @a[2] = Nil;
    is @a.gist, '[4 8 (Any) 16]', 'Nil assigned to a real-array element gists as (Any)';
}

{
    my @a[3];
    is @a.gist, '[(Any) (Any) (Any)]', '1D shaped array cells gist as (Any)';
}

{
    my @a[2,2];
    is @a.gist, "[[(Any) (Any)]\n [(Any) (Any)]]", '2D shaped array cells gist as (Any)';
}

# Typed cells keep their type object, not Any.
{
    my Int @a[3];
    is @a.gist, '[(Int) (Int) (Int)]', 'typed shaped array cells gist as the element type';
}

# Autoviv holes were already rendered as (Any) and stay that way.
{
    my @a;
    @a[2] = 1;
    is @a.gist, '[(Any) (Any) 1]', 'autoviv holes gist as (Any)';
}

# A List keeps a real Nil.
is (1, Nil, 3).gist, '(1 Nil 3)', 'a List preserves a real Nil element in gist';

# Bare Nil still gists as Nil.
is Nil.gist, 'Nil', 'bare Nil.gist is still Nil';
