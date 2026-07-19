use v6;
use Test;

# A trailing comma in a `for` statement modifier builds a 1-element list,
# so `.say for @a,` iterates once over `(@a,)` (the array itemized) rather
# than flattening `@a` into its elements. This matches the parenthesized
# `for (@a,)` form.

plan 7;

# Single array with trailing comma -> one iteration over the whole array.
{
    my @a = 1, 2;
    my @seen;
    @seen.push($_.raku) for @a,;
    is @seen.elems, 1, 'for @a, iterates once';
    is @seen[0], '[1, 2]', 'topic is the whole array, itemized';
}

# Without the trailing comma the array flattens (two iterations).
{
    my @a = 1, 2;
    my @seen;
    @seen.push($_) for @a;
    is @seen.elems, 2, 'for @a flattens (two iterations)';
}

# Scalar with trailing comma is still a single iteration.
{
    my @seen;
    @seen.push($_) for 5,;
    is-deeply @seen, [5], 'for 5, iterates once over (5,)';
}

# A trailing comma after a full list does not add an extra iteration.
{
    my @seen;
    @seen.push($_) for 1, 2,;
    is-deeply @seen, [1, 2], 'for 1, 2, iterates over both elements';
}

# Multiple arrays each iterated as itemized elements.
{
    my @a = 1, 2;
    my @b = 3, 4;
    my @seen;
    @seen.push($_.raku) for @a, @b;
    is-deeply @seen, ['[1, 2]', '[3, 4]'], 'for @a, @b keeps each array whole';
}

# A hash with a trailing comma iterates once over the whole hash.
{
    my %h = a => 1;
    my $count = 0;
    $count++ for %h,;
    is $count, 1, 'for %h, iterates once over the whole hash';
}
