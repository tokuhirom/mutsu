use Test;
plan 10;

# Integer directives dispatch a user .Int method.
{
    class N1 { method Int { 7 } }
    is sprintf("%d", N1.new), "7", '%d dispatches user .Int';
}
{
    class N2 { method Int { 255 } }
    is sprintf("%x", N2.new), "ff", '%x dispatches user .Int';
    is sprintf("%o", N2.new), "377", '%o dispatches user .Int';
    is sprintf("%b", N2.new), "11111111", '%b dispatches user .Int';
}

# Float directives dispatch a user .Numeric method.
{
    class F { method Numeric { 3.5 } }
    is sprintf("%.2f", F.new), "3.50", '%f dispatches user .Numeric';
    is sprintf("%.1e", F.new), "3.5e+00", '%e dispatches user .Numeric';
}

# %d prefers .Int when both .Int and .Numeric exist.
{
    class B { method Int { 9 }; method Numeric { 3.5 } }
    is sprintf("%d", B.new), "9", '%d uses .Int over .Numeric';
}

# Width/precision still apply after coercion.
{
    class N3 { method Int { 42 } }
    is sprintf("%5d", N3.new), "   42", '%d width applies after coercion';
}

# Captured-outer write inside .Int propagates across sprintf calls.
{
    my $calls = 0;
    class C { method Int { $calls++; 1 } }
    my $c = C.new;
    sprintf("%d", $c);
    sprintf("%d", $c);
    ok $calls >= 2, 'user .Int captured-outer write propagates';
}

# Plain numeric args are unaffected.
{
    is sprintf("%d/%.1f", 10, 2.5), "10/2.5", 'plain numeric args unaffected';
}
