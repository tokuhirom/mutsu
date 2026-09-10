use Test;

plan 4;

sub l () { 1, 2 }

# A single scalar index on a computed (stack) target names one element, so its
# assignment rvalue itemizes (like a named single-index assignment). Multi-index
# slices keep the flat list.

{
    my @a;
    sub arr() { @a }
    my $b = 0;
    my @z = (arr()[$b] = l, l);
    is @z.elems, 1, 'foo()[$scalar] = l, l used as rvalue is one itemized element';
}

{
    my @a;
    my @b = (0, 0);
    my $c = 1;
    my @z = (@a[@b[$c,]] = l, l);
    is @z.elems, 1, '@a[@b[$c,]] single computed index is one itemized element';
}

{
    my @a;
    sub arr2() { @a }
    my @z = (arr2()[0, 1] = 10, 20);
    is @z.elems, 2, 'a computed 2-index slice keeps both values';
}

{
    my @a;
    sub arr3() { @a }
    arr3()[0] = 99;
    is @a[0], 99, 'computed single-index assignment still stores correctly';
}
