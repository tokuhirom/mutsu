use v6;
use Test;

# Iterator protocol: push-exactly / push-at-least / push-all / sink-all
# must work on a chained (temporary) receiver, and the push-* family
# returns the delivered count (IterationEnd when the source ran dry first).
# Spec: raku-doc/doc/Type/Iterator.rakudoc

plan 16;

# --- chained temporary receivers (doc examples) ---
{
    my @array;
    is (1 .. Inf).iterator.push-exactly(@array, 3), 3,
        'push-exactly on a temporary iterator returns the count';
    is-deeply @array, [1, 2, 3], 'push-exactly appended the elements';
}

{
    my @array;
    is (1 .. Inf).iterator.push-at-least(@array, 10), 10,
        'push-at-least on a temporary iterator returns the count';
    is @array.elems, 10, 'push-at-least appended at least n elements';
}

{
    my @array;
    is (1 .. 1000).iterator.push-all(@array).gist, 'IterationEnd',
        'push-all on a temporary iterator returns IterationEnd';
    is @array.elems, 1000, 'push-all appended every element';
}

is (1 .. 1000).iterator.sink-all.gist, 'IterationEnd',
    'sink-all on a temporary iterator returns IterationEnd';

# --- variable receivers: return values and cursor advancement ---
{
    my @a;
    my $it = (1 .. 10).iterator;
    is $it.push-exactly(@a, 3), 3, 'push-exactly returns the count, not Nil';
    is $it.push-at-least(@a, 4), 4, 'push-at-least returns the count';
    is-deeply @a, [1, 2, 3, 4, 5, 6, 7], 'cursor advances across push calls';
}

{
    my @a;
    my $it = (1 .. 2).iterator;
    is $it.push-exactly(@a, 5).gist, 'IterationEnd',
        'push-exactly returns IterationEnd when the source runs dry first';
    is-deeply @a, [1, 2], 'the available elements were still appended';
}

{
    my @a;
    my $it = (1 .. 3).iterator;
    is $it.push-exactly(@a, 3), 3,
        'push-exactly returns the count when it drains the source exactly';
}

# --- regression: the rest of the index-advancing family ---
{
    my $it = (1 .. 3).iterator;
    is $it.pull-one, 1, 'pull-one first';
    $it.pull-one;
    is $it.pull-one, 3, 'pull-one third';
    is $it.pull-one.gist, 'IterationEnd', 'pull-one exhausted';
}

done-testing;
