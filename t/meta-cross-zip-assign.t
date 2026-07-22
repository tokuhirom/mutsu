use Test;

# X/Z meta-assignment operators (`@a X[+=] @b`, `@a Z[+=] @b`). The inner op is
# an in-place assignment operator, so each cross (X, left index slowest) or zip
# (Z) pair mutates the corresponding left cell. The left container is written
# back, and the expression value is the Seq of the per-op assignment results.

plan 16;

# --- Cross meta-assignment: @a[i] accumulates every @b element. ---
{
    my @a = 1, 2, 3;
    my @b = 10, 20, 30;
    @a X[+=] @b;
    is-deeply @a, [61, 62, 63], 'X[+=] mutates each left cell by the whole right';
}
{
    my @a = 100, 200;
    my @b = 1, 2, 3;
    @a X[-=] @b;
    is-deeply @a, [94, 194], 'X[-=] subtracts every right element';
}
{
    # The expression value is the Seq of per-op results, distinct from @a for X.
    my @a = 1, 2, 3;
    my @b = 10, 20, 30;
    my @r = @a X[+=] @b;
    is-deeply @r, [11, 31, 61, 12, 32, 62, 13, 33, 63], 'X[+=] value is the per-op result Seq';
    is-deeply @a, [61, 62, 63], 'X[+=] still mutates @a when the value is captured';
}

# --- Zip meta-assignment: element-wise. ---
{
    my @a = 1, 2, 3;
    my @b = 10, 20, 30;
    @a Z[+=] @b;
    is-deeply @a, [11, 22, 33], 'Z[+=] adds element-wise';
}
{
    my @a = 2, 3, 4;
    my @b = 10, 10, 10;
    @a Z[*=] @b;
    is-deeply @a, [20, 30, 40], 'Z[*=] multiplies element-wise';
}
{
    my @a = 2, 3;
    my @b = 3, 2;
    @a Z[**=] @b;
    is-deeply @a, [8, 9], 'Z[**=] exponentiates element-wise';
}
{
    my @a = 5, 5;
    my @b = 3, 9;
    @a Z[min=] @b;
    is-deeply @a, [3, 5], 'Z[min=] takes the element-wise minimum';
}
{
    my @a = 1, 2, 3;
    my @b = 10, 20, 30;
    my @r = @a Z[+=] @b;
    is-deeply @r, [11, 22, 33], 'Z[+=] value equals the mutated cells';
    is-deeply @a, [11, 22, 33], 'Z[+=] mutates @a';
}

# --- String and Unicode inner ops. ---
{
    my @a = "a", "b";
    my @b = "x", "y";
    @a Z[~=] @b;
    is-deeply @a, ["ax", "by"], 'Z[~=] concatenates strings element-wise';
}
{
    my @a = 2, 3;
    my @b = 10;
    @a Z[×=] @b;
    is-deeply @a, [20, 3], 'Z[×=] accepts the Unicode multiply alias';
}

# --- Uneven zip leaves the extra left cells untouched. ---
{
    my @a = 1, 2, 3, 4;
    my @b = 10, 20;
    @a Z[+=] @b;
    is-deeply @a, [11, 22, 3, 4], 'Z stops at the shorter operand';
}

# --- A Range as the right operand. ---
{
    my @a = 0, 0;
    @a X[+=] (1..3);
    is-deeply @a, [6, 6], 'X[+=] iterates a Range right operand';
}

# --- Scalar left operand folds into a single cell. ---
{
    my $a = 5;
    my @b = 1, 2, 3;
    $a X[+=] @b;
    is $a, 11, 'scalar X[+=] folds every right element into the scalar';
}

# --- Plain (non-assign) X still produces the flat cross list. ---
{
    my @a = 1, 2, 3;
    my @b = 10, 20;
    is-deeply (@a X[+] @b).List, (11, 21, 12, 22, 13, 23).List,
        'plain X[+] is unaffected';
}
