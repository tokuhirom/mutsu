use Test;

plan 8;

# .append / .push / .prepend / .unshift on an rvalue Buf (no container to
# write back to) must perform the operation and return the new Buf.
{
    my $r = Buf.new(1, 2).append(3, 4);
    is-deeply $r.list.Array, [1, 2, 3, 4], 'Buf rvalue .append(ints)';
}
{
    my $body = Buf.new(65, 66);
    my $r = Buf.new("X".encode).append: $body;
    is-deeply $r.list.Array, [88, 65, 66], 'Buf rvalue .append: chained colon form';
}
{
    my $r = Buf.new(3, 4).prepend(1, 2);
    is-deeply $r.list.Array, [1, 2, 3, 4], 'Buf rvalue .prepend';
}
{
    my $r = Buf.new(3, 4).unshift(1, 2);
    is-deeply $r.list.Array, [1, 2, 3, 4], 'Buf rvalue .unshift';
}
{
    my $r = Buf.new(1).push(2);
    is-deeply $r.list.Array, [1, 2], 'Buf rvalue .push';
}

# Appending a Blob/Buf argument flattens its bytes.
{
    my $r = Buf.new(1, 2).append(Buf.new(3, 4));
    is-deeply $r.list.Array, [1, 2, 3, 4], 'Buf rvalue .append(Buf)';
}

# The named-variable path still mutates in place.
{
    my $b = Buf.new(1, 2);
    $b.append(3);
    is-deeply $b.list.Array, [1, 2, 3], 'named Buf .append mutates in place';
}

# Blob (immutable) rvalue append throws.
{
    dies-ok { Blob.new(1, 2).append(3) }, 'Blob rvalue .append throws (immutable)';
}
