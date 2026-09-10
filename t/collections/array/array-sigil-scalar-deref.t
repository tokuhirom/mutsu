use Test;

plan 12;

# `@$x` is the positional deref of the SCALAR `$x` — the same thing `@($x)`
# means — and must never resolve to the separate array variable `@x`.

{
    my $b = (1, 2, 3);
    my @b = 9, 9;
    is (@$b).elems, 3, '@$b reads the scalar $b, not the array @b';
    is @b.elems, 2, 'and @b is untouched';
}

# The collision that broke Digest::SHA1: inside a routine, `@$_` used to read
# the implicit slurpy `@_` (arity of the call), not the topic.
{
    sub chunk-sizes($b) {
        (map { (@$_).elems }, $b.rotor(16)).List;
    }
    is chunk-sizes(blob32.new(1 xx 32)), (16, 16), '@$_ in a routine derefs the topic';
}

# A Buf/Blob is Positional, so `@$blob` yields its elements (and therefore
# flattens and slips element-wise).
{
    my $msg = "abc".encode;
    is (@$msg).List, (97, 98, 99), '@$blob is the element list';
    is (flat @$msg, 0x80).List, (97, 98, 99, 128), 'and flattens inside flat';
    is (|@$msg, 0x80).List, (97, 98, 99, 128), 'and slips element-wise';
    # A Blob held in a `$` container is still ONE item without the `@` deref.
    is (flat $msg, 0x80).elems, 2, 'a bare $blob stays a single item in flat';
}

# Mutation goes through the scalar's container.
{
    my $x = [1, 2, 3];
    @$x[0] = 9;
    is $x.List, (9, 2, 3), '@$x[0] = … writes through the scalar';
    @$x.push(4);
    is $x.List, (9, 2, 3, 4), '@$x.push mutates the same array';
    push @$x, 5;
    is $x.List, (9, 2, 3, 4, 5), 'push @$x, … mutates the same array';
}

# `%$h` (already correct) keeps working, and `@$x` matches `@($x)`.
{
    my $h = { a => 1 };
    my %h = (x => 1, y => 2, z => 3);
    is (%$h).elems, 1, '%$h reads the scalar $h';
    my $l = (7, 8);
    is (@$l).List, (@($l)).List, '@$x and @($x) agree';
}
