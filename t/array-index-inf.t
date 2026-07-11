use Test;

# A non-finite positional index (Inf/NaN) must not panic the VM.
# raku throws when converting Inf to an Int index; mutsu must raise a
# catchable exception (not an out-of-bounds Rust panic) and leave the
# array untouched. Regression for S02-types/array.t `@a[Inf] = "dog"`.

plan 6;

{
    my @a = 1..10;
    my $threw = False;
    { @a[Inf] = "dog"; CATCH { default { $threw = True } } }
    ok $threw, 'assigning to @a[Inf] throws instead of panicking';
    is @a.elems, 10, '... and the array is left unmodified';
}

{
    my @a = 1..10;
    my $threw = False;
    { @a[NaN] = "dog"; CATCH { default { $threw = True } } }
    ok $threw, 'assigning to @a[NaN] throws instead of panicking';
    is @a.elems, 10, '... and the array is left unmodified';
}

{
    # A finite in-range index still works after the guard.
    my @a = 1..10;
    @a[3] = 99;
    is @a[3], 99, 'finite index assignment still works';
}

{
    # Autovivifying assignment on a fresh variable with a finite index works.
    my @b;
    @b[5] = 'x';
    is @b[5], 'x', 'autoviv assignment at a finite index still works';
}
