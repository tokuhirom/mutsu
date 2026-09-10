use Test;

plan 14;

# Slice 2c (docs/scalar-array-sharing.md): a value-alias scalar (`my $n = @z`,
# Slice 2a) holds the source array by reference through a shared ContainerRef
# cell. Deref-binding that scalar (`my @a := @$n`) must bind @a to the SAME
# cell, so mutations through @a propagate to $n and the original @z.
#
# The parser conflates `@$n` (deref of scalar $n) with `@n` (an array variable),
# so the bind has to fall back to the same-named scalar's cell when no `@n`
# container variable exists.

{
    my @z = (1, 2);
    my $n = @z;
    my @a := @$n;
    is @a.elems, 2, '@a binds the deref of $n with the right initial contents';
    @a.push(99);
    is @z[*-1], 99, 'push through @a reaches the original @z';
    is $n[*-1], 99, 'push through @a is visible via $n';
    is @a.elems, 3, '@a itself observes the push';
}

# Element assignment through the array deref-bind (not just `.push`) must also
# write through the shared cell. (The element-assign path resolves its target
# through the sigilless alias, which previously pointed at the non-existent
# `@n` container instead of the scalar's shared cell.)
{
    my @z = (1, 2);
    my $n = @z;
    my @a := @$n;
    @a[0] = 99;
    is @z[0], 99, 'element assignment through @a reaches the original @z';
    is $n[0], 99, 'element assignment through @a is visible via $n';
}

# Slice 2c, hash form: `my %h := %$m` shares the cell the value-alias scalar
# `$m` (`my $m = %z`) holds. The parser desugars `%$m` to a `$m.hash` method
# call; in a `:=` bind we rewrite the plain-scalar form to a `HashVar` so it
# takes the same deref-bind path the array form (`@$n`) already uses.
{
    my %z = (a => 1);
    my $m = %z;
    my %h := %$m;
    is %h<a>, 1, '%h binds the deref of $m with the right initial contents';
    %h<x> = 99;
    is %z<x>, 99, 'element assignment through %h reaches the original %z';
    is $m<x>, 99, 'element assignment through %h is visible via $m';
    %z<y> = 2;
    is %h<y>, 2, 'a write to %z is visible through %h (bidirectional)';
}

# A real %n hash variable still takes precedence over the scalar fallback.
{
    my %n = (k => 10);
    my $n = { other => 1 };
    my %h := %n;
    %h<m> = 20;
    is %n<m>, 20, 'bind to the real %n hash variable, not the scalar $n';
    is $n<other>, 1, '$n scalar is untouched by the %n bind';
}

# A real @n array variable still takes precedence over the scalar fallback.
{
    my @n = (10, 20);
    my $n = [1, 2, 3];
    my @a := @n;
    @a.push(30);
    is @n, [10, 20, 30], 'bind to the real @n array variable, not the scalar $n';
    is $n.elems, 3, '$n scalar is untouched by the @n bind';
}
