use Test;

plan 12;

# `@a[$x;$y] op= rhs` — the multi-dimensional subscript as a compound-assignment
# lvalue. The parser had no arm for it, so it fell through to the generic
# "not an lvalue" case and compiled to an unconditional X::Assignment::RO:
# every `@lanes[$x;$y] +^= @D[$x]` (Digest::SHA3's `KeccakF1600`) died with
# "Cannot modify an immutable value", while the plain `@a[$x;$y] = v` form has
# always worked.

{
    my @a = [1, 2], [3, 4];
    @a[0;1] += 5;
    is @a.raku, [[1, 7], [3, 4]].raku, 'numeric compound assign through @a[x;y]';

    @a[1;0] +^= 5;
    is @a.raku, [[1, 7], [6, 4]].raku, 'bitwise compound assign through @a[x;y]';

    @a[1;1] ~= "!";
    is @a[1;1], "4!", 'string compound assign through @a[x;y]';
}

# Three dimensions.
{
    my @c;
    @c[0;0;0] = 1;
    @c[0;0;0] += 41;
    is @c[0;0;0], 42, 'compound assign through a three-dimensional subscript';
}

# An `@`-parameter is bound read-only, but its ELEMENTS are the caller's
# containers and stay writable — this is what Digest::SHA3 relies on.
{
    sub bump(@lanes) { @lanes[1;1] +^= 5; @lanes }
    my @b = [1, 2], [3, 4];
    bump(@b);
    is @b.raku, [[1, 2], [3, 1]].raku, 'compound assign through an @-parameter element';

    multi mbump(@lanes) { @lanes[0;0] +^= 1; @lanes }
    my @d = [1, 2], [3, 4];
    mbump(@d);
    is @d.raku, [[0, 2], [3, 4]].raku, '...and through a multi candidate';
}

# A hash of hashes uses the same subscript form.
{
    my %g = a => { b => 1 };
    %g{"a";"b"} += 41;
    is %g<a><b>, 42, 'compound assign through %h{k1;k2}';
}

# The subscript expressions must be evaluated exactly once, shared by the
# read-back and the write.
{
    my $calls = 0;
    sub idx($n) { $calls++; $n }
    my @e = [1, 2], [3, 4];
    @e[idx(0); idx(1)] += 10;
    is @e[0;1], 12, 'the element is updated';
    is $calls, 2, 'each subscript expression is evaluated exactly once';
}

# Short-circuit compound operators keep their short-circuit semantics.
{
    my @f = [1, Nil], [3, 4];
    @f[0;1] //= 9;
    is @f[0;1], 9, '//= fills an undefined element';
    @f[0;1] //= 11;
    is @f[0;1], 9, '//= leaves a defined element alone';
}

# The whole thing is still an expression yielding the assigned value.
{
    my @g = [1, 2], [3, 4];
    my $r = (@g[0;0] += 6);
    is $r, 7, 'the compound assignment evaluates to the new value';
}
