use Test;

# `for @a { @a.push(...) }` iterates the LIVE array: elements appended to the
# source during the loop are picked up and iterated, matching Rakudo. mutsu used
# to materialise the iteration list once and stop at the original length.
# (roast/S04-statements/for.t tests 82/83, RT113026.)

plan 8;

# The canonical RT113026 case.
{
    my @a = 1 .. 10;
    my $iter = 0;
    for @a -> $n {
        $iter++;
        @a.push: $n if $iter % 2;
    }
    is $iter, 20, 'iterating over an expanding list runs for every appended item';
    is-deeply @a, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 3, 5, 7, 9, 1, 5, 9, 5, 5],
        'the array expanded in the for loop is fully expanded';
}

# Topic form (`$_`) also iterates the live array.
{
    my @b = 1, 2;
    my $count = 0;
    for @b {
        $count++;
        @b.push(0) if $count < 4;   # bounded so it terminates
    }
    is $count, 5, 'topic for over an expanding array picks up appended items';
}

# `last` stops the whole loop even though the array is still growing.
{
    my @c = 1, 2, 3;
    my $c = 0;
    for @c -> $n {
        $c++;
        @c.push(99);
        last if $c >= 3;
    }
    is $c, 3, 'last stops the expanding loop';
    is @c.elems, 6, 'last leaves the array with only the in-loop pushes';
}

# `return` from inside the loop exits without continuing into appended items.
{
    sub f { my @d = 1, 2, 3; for @d -> $n { @d.push(9); return $n if $n == 2 } }
    is f(), 2, 'return from an expanding for loop exits immediately';
}

# A non-growing plain `for @a` is unaffected.
{
    my @e = 1, 2, 3;
    my $sum = 0;
    $sum += $_ for @e;
    is $sum, 6, 'a non-growing for loop iterates exactly once per element';
}

# Iterating a literal list (not a single array var) is unaffected.
{
    my $n = 0;
    for (1, 2, 3) { $n++ }
    is $n, 3, 'for over a literal list is not treated as a live array';
}
