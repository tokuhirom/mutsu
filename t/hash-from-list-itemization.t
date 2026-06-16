use Test;

plan 14;

# A bare `%h` in a list flattens into its pairs on hash-assignment / hash-context,
# but a hash sourced from a `$` scalar container is itemized and stays opaque.
# (Container itemization: distinguishes `(%h,)` from `($hashitem,)`.)

my %h = a => 1, b => 2;

# Single-element list with a bare hash -> flattens.
{
    my %c = (%h,);
    is %c.elems, 2, '(%h,) flattens into pairs (elems)';
    is %c<a>, 1, '(%h,) flatten keeps value a';
    is %c<b>, 2, '(%h,) flatten keeps value b';
}

# Hash-context coercion `%(...)` of a bare hash -> flattens.
{
    my $r = %(%h,);
    is $r<a>, 1, '%(%h,) flattens (a)';
    is $r<b>, 2, '%(%h,) flattens (b)';
}

# Multi-element list with a bare hash -> flattens alongside other pairs.
{
    my %m = %h, c => 3;
    is %m.elems, 3, '%h, c => 3 flattens %h and adds c';
    is %m<c>, 3, 'extra pair present';
    is %m<a>, 1, 'flattened pair present';
}

# A `$`-sourced (itemized) hash in a single-element list must NOT flatten:
# Raku raises X::Hash::Store::OddNumber.
{
    my $hi = %h;
    dies-ok { my %c = ($hi,) }, '($hashitem,) does not flatten (odd number)';
}

# Array assignment keeps a hash as a single element either way.
{
    my $hi = %h;
    is (my @a = (%h,)).elems, 1, '@a = (%h,) is one element';
    is (my @b = ($hi,)).elems, 1, '@a = ($hi,) is one element';
    is @b[0]<a>, 1, 'itemized hash element is intact';
}

# Explicit itemization `$(...)` also stays opaque.
{
    my $hi = %h;
    is $hi.VAR.^name, 'Scalar', '$-sourced hash reflects as Scalar container';
    is %h.VAR.^name, 'Hash', '%-sourced hash reflects as Hash';
}
