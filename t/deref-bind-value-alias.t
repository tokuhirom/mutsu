use Test;

plan 6;

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

# NOTE: the hash deref-bind form (`my %m := %$h`) is NOT covered here. The
# parser desugars `%$h` to a `$h.hash` method call (not a simple `HashVar`, the
# way `@$n` becomes `ArrayVar("n")`), so it takes a different bind path. Sharing
# through a hash deref-bind is tracked as remaining work in
# docs/scalar-array-sharing.md.

# A real @n array variable still takes precedence over the scalar fallback.
{
    my @n = (10, 20);
    my $n = [1, 2, 3];
    my @a := @n;
    @a.push(30);
    is @n, [10, 20, 30], 'bind to the real @n array variable, not the scalar $n';
    is $n.elems, 3, '$n scalar is untouched by the @n bind';
}
