use Test;

plan 4;

sub l () { 1, 2 }

# A slice assignment's rvalue is the list of values actually assigned (truncated
# to the number of slots), not the whole RHS.

{
    my @a;
    sub arr() { @a }
    my @z = (arr()[0, 1] = 10, 20, 30);
    is @z.elems, 2, 'computed 2-slot slice rvalue keeps only the 2 assigned values';
    is ~@z, '10 20', '... which are the first two RHS values';
}

# A 1-element computed slice yields one itemized element.
{
    my @a;
    sub arr2() { @a }
    my $b = 0;
    my @z = (arr2()[$b,] = flat l, l);
    is @z.elems, 1, 'a 1-slot computed slice yields one element';
    ok !@z[1].defined, '... with nothing after it';
}
