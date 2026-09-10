use Test;

plan 6;

# Whatever (*) array slice assignment: @n[*] means all indices
{
    my @n = 0, 1;
    @n[*] = <a b>.sort;
    is @n, <a b>, 'Whatever array slice assigned from Seq works';
}

{
    my @n = 0, 1, 2;
    @n[*] = <a b>.sort;
    is @n, ("a", "b", Any), 'Whatever array slice Nils unassigned indices';
}

# Trailing comma in subscript creates a slice (list of indices)
{
    my @n = 0, 1;
    @n[0,] = <a b>.sort;
    is @n, ("a", 1), 'single index array slice assignment from Seq (trailing comma)';
}

{
    my @n = 10, 20, 30;
    @n[1,] = 99;
    is @n, (10, 99, 30), 'trailing comma slice assigns single value';
}

# Multiple indices still work normally
{
    my @n = 0, 1, 2;
    @n[0, 2] = <a b>;
    is @n, ("a", 1, "b"), 'multi-index slice assignment works';
}

# Whatever with empty array
{
    my @n;
    @n[*] = <a b>;
    is @n.elems, 0, 'Whatever on empty array assigns nothing';
}
