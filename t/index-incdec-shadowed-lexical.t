use Test;

plan 8;

# `$c[i]++` located its base container by NAME in `code.locals`, which picks the
# FIRST slot carrying that name. A bare block shares its enclosing frame's
# locals, so an inner `my $c` shadowing an outer one got a second slot under the
# same name — and the increment landed in the OUTER container.

{
    my $c = [7, 8, 9];
    {
        my $c = [0, 3];
        $c[0]++;
        is $c.List, (1, 3), 'postfix ++ on an index hits the shadowing lexical';
    }
    is $c.List, (7, 8, 9), 'and leaves the outer container alone';
}

{
    my $c = [7, 8, 9];
    {
        my $c = [0, 3];
        $c[1]--;
        is $c.List, (0, 2), 'postfix -- likewise';
    }
    is $c.List, (7, 8, 9), 'outer untouched';
}

{
    my $c = [7, 8, 9];
    {
        my $c = [0, 3];
        ++$c[0];
        --$c[1];
        is $c.List, (1, 2), 'prefix ++/-- likewise';
    }
    is $c.List, (7, 8, 9), 'outer untouched';
}

# A `:=`-bound outer is the shape roast/integration/advent2013-day08.t hits.
{
    my $c := [7, 8, 9];
    {
        my $c = [0, 3];
        $c[0]++;
        is $c.List, (1, 3), 'a `:=`-bound outer does not capture the increment';
    }
}

# Hash subscripts share the code path.
{
    my $h = { a => 10 };
    {
        my $h = { a => 1 };
        $h<a>++;
        is $h<a>, 2, 'hash subscript ++ hits the shadowing lexical';
    }
}
