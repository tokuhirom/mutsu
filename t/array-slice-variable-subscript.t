use v6;
use Test;

plan 29;

# A subscript selects a slice or a single element according to the *runtime
# value* of the subscript (Rakudo dispatches postcircumfix:<[ ]> on Iterable),
# not according to the syntax that produced it. A `for` loop must therefore
# iterate `@a[$range]` element-wise exactly like `@a[0..2]`.

my @numbers = <4 8 15 16 23 42>;

# --- literal Range subscript -------------------------------------------------
{
    my @seen;
    @seen.push($_) for @numbers[0..2];
    is @seen.elems, 3, 'literal Range subscript iterates 3 times';
    is @seen.join(','), '4,8,15', 'literal Range subscript yields the slice elements';
    is @numbers[0..2].elems, 3, 'literal Range slice has 3 elements';
}

# --- Range held in a scalar --------------------------------------------------
{
    my $range := 0..2;
    my @seen;
    @seen.push($_) for @numbers[$range];
    is @seen.elems, 3, 'bound Range subscript iterates 3 times';
    is @seen.join(','), '4,8,15', 'bound Range subscript yields the slice elements';
    is @numbers[$range].elems, 3, 'bound Range slice has 3 elements';

    # An explicitly itemized Range is a single index that numifies to its
    # element count (3), not a slice.
    my @seen2;
    @seen2.push($_) for @numbers[my $ = 1..3];
    is @seen2.elems, 1, 'itemized Range subscript is a single index, not a slice';
    is @seen2[0], 16, 'itemized Range subscript numifies to its element count';
}

# --- Array of indices --------------------------------------------------------
{
    my @range = 0..2;
    my @seen;
    @seen.push($_) for @numbers[@range];
    is @seen.elems, 3, 'Array subscript iterates 3 times';
    is @seen.join(','), '4,8,15', 'Array subscript yields the slice elements';
    is @numbers[@range].elems, 3, 'Array slice has 3 elements';

    my @sparse = 0, 3, 5;
    my @seen2;
    @seen2.push($_) for @numbers[@sparse];
    is @seen2.join(','), '4,16,42', 'non-contiguous Array subscript iterates element-wise';
}

# --- a list literal subscript ------------------------------------------------
{
    my @seen;
    @seen.push($_) for @numbers[(0, 1)];
    is @seen.join(','), '4,8', 'list-literal subscript iterates element-wise';

    my $indices := (0, 1);
    my @seen2;
    @seen2.push($_) for @numbers[$indices];
    is @seen2.join(','), '4,8', 'bound list subscript iterates element-wise';
}

# --- a single Int index must NOT flatten -------------------------------------
{
    my $i = 1;
    my @seen;
    @seen.push($_) for @numbers[$i];
    is @seen.elems, 1, 'single Int variable subscript iterates exactly once';
    is @seen[0], 8, 'single Int variable subscript topicalizes the element';

    my @seen2;
    @seen2.push($_) for @numbers[2];
    is @seen2.elems, 1, 'single Int literal subscript iterates exactly once';
}

# An element that is *itself* a list must still be a single iteration.
{
    my @nested = ([1, 2, 3], [4, 5]);
    my $j = 0;
    my $iterations = 0;
    $iterations++ for @nested[$j];
    is $iterations, 1, 'element holding an Array iterates once, not per inner element';

    my $iterations2 = 0;
    $iterations2++ for @nested[0];
    is $iterations2, 1, 'literal index onto an Array element iterates once';
}

# --- Hash slices -------------------------------------------------------------
{
    my %h = a => 1, b => 2, c => 3;
    my @keys = <a b>;
    my @seen;
    @seen.push($_) for %h{@keys};
    is @seen.elems, 2, 'Hash slice by Array of keys iterates twice';
    is @seen.sort.join(','), '1,2', 'Hash slice by Array of keys yields both values';

    my $key = 'c';
    my @seen2;
    @seen2.push($_) for %h{$key};
    is @seen2.elems, 1, 'single Hash key in a variable iterates once';
    is @seen2[0], 3, 'single Hash key in a variable topicalizes the value';

    is %h{@keys}.elems, 2, 'Hash slice has 2 elements';
}

# --- rw aliasing of a single element is preserved ----------------------------
{
    my @a = 1, 2, 3, 4;
    my $i = 2;
    for @a[$i] { $_ = $_ * 10 }
    is @a.join(','), '1,2,30,4', 'single element subscripted by a variable aliases rw';

    my @b = 1, 2, 3, 4;
    for @b[1] { $_ = $_ * 10 }
    is @b.join(','), '1,20,3,4', 'single element subscripted by a literal aliases rw';

    my %h = a => [1, 2, 3];
    for %h<a> { .push(4) }
    is %h<a>.join(','), '1,2,3,4', 'Hash element holding an Array is mutated in place';
}

# --- a slice loop must not write the topic back over the range ---------------
{
    my @c = 1, 2, 3, 4;
    my $r := 0..1;
    my $n = 0;
    $n++ for @c[$r];
    is $n, 2, 'read-only slice loop over a bound Range runs twice';
    is @c.join(','), '1,2,3,4', 'read-only slice loop leaves the array untouched';
}
