use Test;

plan 11;

# `++`/`--` on a subscript with a WhateverCode index (`@a[*-1]++`) used to
# stringify the raw closure into a bogus key and lose the update. The index must
# be resolved against the container length first, like the `+=` path already did.

{
    my @a = 1, 2, 3;
    @a[*-1]++;
    is-deeply @a, [1, 2, 4], 'postfix ++ on @a[*-1] increments the last element';
}
{
    my @a = 1, 2, 3;
    @a[*-2]++;
    is-deeply @a, [1, 3, 3], '@a[*-2]++ targets the correct element';
}
{
    my @a = 1, 2, 3;
    ++@a[*-1];
    is-deeply @a, [1, 2, 4], 'prefix ++ on @a[*-1] increments';
}
{
    my @a = 1, 2, 3;
    @a[*-1]--;
    is-deeply @a, [1, 2, 2], 'postfix -- on @a[*-1] decrements';
}
{
    my @a = 1, 2, 3;
    my $old = @a[*-1]++;
    is $old, 3, 'postfix ++ returns the OLD value';
    is-deeply @a, [1, 2, 4], '...and mutates the element';
}
{
    my @a = 5, 5, 5;
    @a[*-1]++ for 1 .. 3;
    is-deeply @a, [5, 5, 8], 'repeated @a[*-1]++ accumulates';
}
{
    my @a = 1, 2, 3;
    my $i = *-1;
    @a[$i]++;
    is-deeply @a, [1, 2, 4], 'a WhateverCode stored in a variable also resolves';
}

# a literal index and a hash key are unaffected
{
    my @a = 1, 2, 3;
    @a[2]++;
    is-deeply @a, [1, 2, 4], 'literal index ++ still works';
}
{
    my %h = a => 1;
    %h<a>++;
    is-deeply %h, {a => 2}, 'hash-key ++ still works';
}
{
    my @a = 1, 2, 3;
    @a[0]++;
    is-deeply @a, [2, 2, 3], 'first-element ++ still works';
}
