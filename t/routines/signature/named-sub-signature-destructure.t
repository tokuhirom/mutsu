use Test;

plan 6;

sub positional-fixed(@a [$first, $second]) {
    [@a, $first, $second]
}
is-deeply positional-fixed([1, 2]), [[1, 2], 1, 2],
    'positional destructuring without a slurpy still works';

sub positional-slurpy(@a [$first, *@rest]) {
    [$first, @rest, @rest.^name]
}
is-deeply positional-slurpy([1, 2, 3]), [1, [2, 3], 'Array'],
    'positional destructuring keeps a slurpy as an Array';

sub named-fixed(:@a [$first, $second]) {
    [@a, $first, $second]
}
is-deeply named-fixed(a => [1, 2]), [[1, 2], 1, 2],
    'named destructuring without a slurpy binds each sub-parameter';

sub named-slurpy(:@a [$first, *@rest]) {
    [$first, @rest, @rest.^name]
}
is-deeply named-slurpy(a => [1, 2, 3]), [1, [2, 3], 'Array'],
    'named destructuring binds its slurpy to the remaining elements';

sub named-full(:@a [$first, *@rest, :$named, *%restnameds]) {
    [@a, $first, @rest, $named, %restnameds]
}
is-deeply named-full(a => [1, 2, 3, named => 9, extra => 8]),
    [[1, 2, 3, named => 9, extra => 8], 1, [2, 3], 9, {extra => 8}],
    'named destructuring also binds inner named parameters and named slurpies';

our @trait-seen;
multi trait_mod:<is>(Variable $a, :@foo [$firstpos, *@restpos]) {
    @trait-seen = [$firstpos, @restpos]
}
my $x is foo[1, 2, 3] = 1;
is-deeply @trait-seen, [1, [2, 3]],
    'trait_mod receives and destructures a named array parameter';
