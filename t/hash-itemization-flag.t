use Test;

plan 18;

# A bare `%h` in a list flattens into its pairs on hash-assignment / hash-context,
# but a hash sourced from a `$` scalar carries HashData.itemized and stays opaque.
# (Container itemization mirrors ArrayKind: the value stays a Value::Hash, so it
# never leaks a wrapper to value operations — `(elem)`, callable mappers, etc.)

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

# No leak: an itemized hash stays a usable Value::Hash for value operations.
{
    my $hi = %h;
    # membership over a hash held in a list element
    my @containers = $hi,;
    ok 'a' (elem) @containers[0], '(elem) sees a $-sourced hash in a list (no leak)';
    # subscript a hash that has lived in a list element
    is @containers[0]<b>, 2, 'subscript on a $-sourced hash in a list (no leak)';
}

# Itemization flag does not affect hash equality.
{
    my $hi = %h.item;
    is-deeply $hi, %h, 'an itemized hash is is-deeply-equal to the bare hash';
}

# An itemized hash does NOT flatten into pairs in array/list context (mirrors
# itemized arrays). A bare `%h` in `[...]` flattens to its pairs.
{
    my %b = 'x' => 42;
    is [item %b].elems, 1, '[item %h] keeps the hash as one element';
    is-deeply [item %b], [${'x' => 42}], 'item %hash non-flattening matches $(...)';
}

# Reflection: a $-sourced hash reflects as a Scalar container, % as Hash.
{
    my $hi = %h;
    is $hi.VAR.^name, 'Scalar', '$-sourced hash reflects as Scalar container';
    is %h.VAR.^name, 'Hash', '%-sourced hash reflects as Hash';
}
