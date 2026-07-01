use Test;

plan 4;

sub l () { 1, 2 }

# In a `*`-slurpy parenthesized list assignment, a single scalar target takes
# exactly ONE item (the one at its position), not the whole list.
{
    my $a;
    my @z = (($a, *) = flat l, l, l);
    is $a.elems, 1, '($a, *) = flat l, l, l gives $a exactly one item';
}

{
    my $a;
    ($a, *) = 10, 20, 30;
    is $a, 10, '($a, *) statement form takes the first item';
}

# A leading `*` discards, then the scalar takes the next item.
{
    my $a;
    (*, $a) = 10, 20, 30;
    is $a, 20, '(*, $a) skips the first item';
}

# A trailing `@` slurpy still slurps the tail.
{
    my @b;
    (*, @b) = 1, 2, 3, 4;
    is ~@b, '2 3 4', '(*, @b) slurps the tail after the discard';
}
