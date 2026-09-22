use Test;

# #9033: the UNBRACKETED spelling of an X/Z meta-assignment (`@a X+= @b`)
# lowered to `@a = (@a X+ @b)` -- the meta-operator applied to the plain infix,
# with an assignment wrapped around it -- instead of the meta-operator applied
# to the ASSIGNMENT infix `+=`, which accumulates into the left cells in place
# and keeps the left container's length.
#
# The bracketed spelling `@a X[+=] @b` was already right (it is pinned next
# door in meta-cross-zip-assign.t); these are the same operators written the
# other way, and Raku makes no distinction between them.
#
# `Z+=` looked correct before the fix only because `@a = (@a Z+ @b)` happens to
# agree with element-wise accumulation when the two sides are the same length.
# The uneven cases below are where it did not, so they are the ones that would
# catch a relapse.

plan 18;

# --- Cross: each left cell accumulates every right element, left index
# slowest. The left container keeps its length; before the fix it grew to
# elems * elems. ---
{
    my @d = 1, 2;
    @d X+= (10, 20);
    is-deeply @d, [31, 32], 'X+= accumulates every right element into each left cell';
}
{
    my @e = 1, 2;
    my @f = 10, 20;
    @e X+= @f;
    is-deeply @e, [31, 32], 'X+= takes an array right operand';
}
{
    my @a = 100, 200;
    @a X-= (1, 2, 3);
    is-deeply @a, [94, 194], 'X-= subtracts every right element';
}
{
    my @a = 2, 3;
    @a X*= (10, 20);
    is-deeply @a, [400, 600], 'X*= multiplies by every right element';
}
{
    my @a = "a", "b";
    @a X~= ("x", "y");
    is-deeply @a, ["axy", "bxy"], 'X~= concatenates every right element in turn';
}
{
    my @a = 1, 2, 3;
    @a X+= (10, 20);
    is-deeply @a, [31, 32, 33], 'X+= leaves the left length alone when the sides differ';
}
{
    my @a = 0, 0;
    @a X+= (1..3);
    is-deeply @a, [6, 6], 'X+= iterates a Range right operand';
}
{
    my @a = 1, 2;
    @a X+= ();
    is-deeply @a, [1, 2], 'X+= with an empty right operand leaves the left untouched';
}

# --- The expression value is the Seq of per-op results, which for X differs
# from the mutated container. ---
{
    my @a = 1, 2, 3;
    my @r = @a X+= (10, 20);
    is-deeply @r, [11, 31, 12, 32, 13, 33], 'X+= evaluates to the per-op result Seq';
    is-deeply @a, [31, 32, 33], 'X+= still mutates the left when its value is captured';
}

# --- Zip: element-wise, and the left keeps the cells the zip does not reach.
# That last part is what the old lowering dropped. ---
{
    my @a = 1, 2, 3;
    @a Z+= (10, 20, 30);
    is-deeply @a, [11, 22, 33], 'Z+= adds element-wise';
}
{
    my @a = 1, 2, 3;
    @a Z+= (10, 20);
    is-deeply @a, [11, 22, 3], 'Z+= leaves the left cells the shorter right does not reach';
}
{
    my @a = 1, 2;
    @a Z+= (10, 20, 30);
    is-deeply @a, [11, 22], 'Z+= ignores right elements past the end of the left';
}
{
    my @a = 2, 3;
    @a Z**= (3, 2);
    is-deeply @a, [8, 9], 'Z**= exponentiates element-wise';
}
{
    my @a = 2, 3;
    @a Z×= (10,);
    is-deeply @a, [20, 3], 'Z×= accepts the Unicode multiply alias';
}

# --- Scalar left operand folds every right element into the one cell. ---
{
    my $s = 1;
    $s X+= (10, 20);
    is $s, 31, 'scalar X+= folds every right element into the scalar';
}
{
    my $s = 1;
    $s Z+= 10;
    is $s, 11, 'scalar Z+= is the plain compound assignment';
}

# --- The plain (non-assignment) meta-operator must keep producing the flat
# cross list; the fix must not reach it. ---
{
    my @a = 1, 2, 3;
    is-deeply (@a X+ (10, 20)).List, (11, 21, 12, 22, 13, 23).List,
        'plain X+ still produces the flat cross';
}
