use v6;
use Test;
use lib 't/lib';

plan 7;

# A module outside every package exports its `our` variables and constants as
# `GLOBAL` ones. A block-scoped `use` declares them in the block, over a
# same-named lexical of the caller (this used to trip the env/slot invariant).
my $hat-var = 'caller';
my @hat-list = 9;
my %hat-map = z => 9;

{
    use GlobalOurExportVars;
    is $hat-var, 'trait-var', 'the imported scalar hides the caller\'s';
    is hat-term, 'trait-term', 'the imported constant is a term of the block';
    is-deeply @hat-list, [1, 2, 3], 'the imported array hides the caller\'s';
    is-deeply %hat-map, {a => 1}, 'the imported hash hides the caller\'s';
}

is $hat-var, 'caller', 'the caller\'s scalar is untouched outside the block';
is-deeply @hat-list, [9], 'the caller\'s array is untouched outside the block';
is-deeply %hat-map, {z => 9}, 'the caller\'s hash is untouched outside the block';
