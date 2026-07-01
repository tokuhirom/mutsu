use Test;

plan 4;

sub l () { 1, 2 }

# A 1-element array slice (`@a[0,]`) names a single scalar slot, so its rvalue
# itemizes like a single-index assignment -- a following list context sees one
# element.
{
    my @a;
    my @z = (@a[0,] = l, l);
    is @z.elems, 1, '@a[0,] = l, l used as an rvalue is one itemized element';
    ok @a[0,].elems == 1, 'the 1-element slice touched exactly one slot';
    ok !@a[1].defined, 'the second slot is untouched';
}

# A multi-element slice keeps the flat list as its value.
{
    my @a;
    my @z = (@a[0, 1] = 10, 20);
    is @z.elems, 2, 'a 2-element slice rvalue keeps both values';
}
