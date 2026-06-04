use Test;

# A hyper meta-assignment operator (`»+=«`, `»*=»`, `»~=»`, ...) mutates its
# left-hand lvalue *and* yields the new value as the expression result, so
# `my @r = @a »+=« (...)` binds @r to the updated contents. Previously the
# write-back consumed the stack value and the expression returned Any.
# Mirrors roast/S03-metaops/hyper.t "»+=« / »*=» returns the right value".

plan 8;

{
    my @array = 3, 8, 2, 9, 3, 8;
    my @r = @array »+=« (1, 2, 3, 4, 5, 6);
    is-deeply @r, [4, 10, 5, 13, 8, 14], '»+=« returns the right value';
    is-deeply @array, [4, 10, 5, 13, 8, 14], '»+=« changes its lvalue';
}

{
    my @array = 3, 8, 2, 9, 3, 8;
    my @r = @array »*=» (1, 2, 3);
    is-deeply @r, [3, 16, 6, 9, 6, 24], '»*=» returns the right value';
    is-deeply @array, [3, 16, 6, 9, 6, 24], '»*=» changes its lvalue';
}

{
    my @a = <a b c>;
    my @r = @a »~=» <pie tart>;
    is-deeply @r, ['apie', 'btart', 'cpie'], '»~=» returns the right value';
    is-deeply @a, ['apie', 'btart', 'cpie'], '»~=» changes its lvalue';
}

{
    # scalar lvalue
    my $x = 5;
    my $y = ($x »+=» 3);
    is $y, 8, 'scalar hyper meta-assign returns the new value';
    is $x, 8, 'scalar hyper meta-assign mutates the lvalue';
}
