use Test;

plan 16;

# `@outer.push(@inner)` stores @inner by reference (Raku's non-flattening `**@`
# slurpy), so later mutations of @inner propagate to the stored element.
{
    my @inner = 1, 2, 3;
    my @outer;
    @outer.push(@inner);
    is @outer.elems, 1, 'array pushed as a single element';
    @inner.push(4);
    is-deeply @outer[0], [1, 2, 3, 4], 'later .push on source propagates';
}

# Element assignment on the source propagates.
{
    my @inner = 1, 2, 3;
    my @outer;
    @outer.push(@inner);
    @inner[0] = 100;
    is-deeply @outer[0], [100, 2, 3], 'source element assignment propagates';
}

# Whole-container assignment on the source propagates (mutates in place).
{
    my @inner = 1, 2, 3;
    my @outer;
    @outer.push(@inner);
    @inner = (9, 9);
    is-deeply @outer[0], [9, 9], 'source reassignment propagates';
}

# Pushing a hash by reference.
{
    my %h = a => 1;
    my @o;
    @o.push(%h);
    %h<b> = 2;
    is-deeply @o[0], {a => 1, b => 2}, 'hash pushed by reference propagates';
}

# Two distinct arrays keep distinct references.
{
    my @a = 1, 2;
    my @b = 3, 4;
    my @c;
    @c.push(@a);
    @c.push(@b);
    @a.push(100);
    is-deeply @c[0], [1, 2, 100], 'first reference tracks @a';
    is-deeply @c[1], [3, 4], 'second reference tracks @b';
}

# The source variable still behaves normally after being pushed.
{
    my @inner = 1, 2, 3;
    my @outer;
    @outer.push(@inner);
    is @inner.elems, 3, 'source elems intact';
    is @inner[1], 2, 'source index read intact';
    is @inner.sum, 6, 'source sum intact';
    @inner.pop;
    is-deeply @inner, [1, 2], 'source pop intact';
    is-deeply @outer[0], [1, 2], 'pop propagates to stored element';
}

# Operations on the outer array deref the stored reference correctly.
{
    my @inner = 1, 2, 3;
    my @outer;
    @outer.push(@inner);
    @outer.push(10);
    is @outer.elems, 2, 'mixed reference + scalar elems';
    is $@outer[0].sum, 6, 'stored reference sums correctly';
    is @outer.map({ .WHAT.^name }).join(','), 'Array,Int', 'element types preserved';
}

# Pushing a scalar (not a bare container var) is unaffected (snapshot semantics).
{
    my $s = [9, 9];
    my @c;
    @c.push($s);
    $s = [0];
    is-deeply @c[0], [9, 9], 'scalar push is not reference-promoted';
}
