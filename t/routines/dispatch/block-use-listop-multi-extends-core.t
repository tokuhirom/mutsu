use Test;
use lib 't/lib';

plan 3;

{
    use ListopMultiExtendsCore;
    my @a = (1, 2, 3, 4, 5);
    splice(@a, 1, 2);
    is-deeply @a, [1, 4, 5], 'a block-scoped imported splice multi keeps the core array form';
    is splice("", 0, "Raku"), 'custom  0 Raku', 'and the imported candidate is reachable';
}

my @b = (1, 2, 3);
splice(@b, 0, 1);
is-deeply @b, [2, 3], 'outside the block only the core form exists';
