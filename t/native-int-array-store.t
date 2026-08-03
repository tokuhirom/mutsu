use Test;

plan 11;

# A push onto a native integer array stores through the native slot, so the
# value wraps to the element width exactly as an assignment does. Digest::SHA1's
# message schedule (`@W.push: S(1, …)`) relies on the uint32 truncation.
{
    my uint32 @a;
    @a.push(6535351809);
    is @a[0], 2240384513, 'push onto a uint32 array wraps to 32 bits';

    my uint32 @b = 6535351809;
    is @b[0], 2240384513, 'assignment wraps the same way';

    my uint8 @c;
    @c.push(300);
    is @c[0], 44, 'push onto a uint8 array wraps to 8 bits';

    my int8 @d;
    @d.push(200);
    is @d[0], -56, 'push onto an int8 array wraps signed';

    my uint8 @e;
    @e.push(1, 300, 2);
    is @e.List, (1, 44, 2), 'every pushed element wraps';

    # A boxed array is unaffected.
    my Int @f;
    @f.push(6535351809);
    is @f[0], 6535351809, 'a boxed Int array does not wrap';
}

# Assigning a Buf/Blob to a NATIVE typed array spreads it element-wise
# (`my uint32 @W = $M` in Digest::SHA1's sha1-block).
{
    my $M = blob32.new(1, 2, 3);
    my uint32 @w = $M;
    is @w.elems, 3, 'a blob32 assigned to a uint32 array spreads element-wise';
    is @w.List, (1, 2, 3), 'with the buffer elements';

    my int @i = "abc".encode;
    is @i.List, (97, 98, 99), 'a utf8 Blob spreads into an int array too';

    # An untyped `@` array still sees the itemized Blob as one element.
    my @plain = $M;
    is @plain.elems, 1, 'an untyped array keeps the itemized Blob as one element';
}

{
    my uint32 @w = blob32.new(0x67452301, 0xEFCDAB89);
    is @w[1], 0xEFCDAB89, 'large uint32 values survive the spread';
}
