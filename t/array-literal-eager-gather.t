# An array literal `[...]` is eager: a gather/take LazyList element must be run
# at construction time so its side effects happen and its elements materialize.
# Previously `[ gather {...} ]` stored the unrun LazyList and `value_to_list`
# read its (empty) cache, yielding `[]` and — when the gather mutated an outer
# array a surrounding `while` looped on — an infinite loop.
use Test;

plan 6;

# Direct gather in an array literal flattens its taken values.
is-deeply [ gather { take 1; take 2; take 3 } ], [1, 2, 3],
    'array literal flattens a single gather element';

# The gather body runs at array-build time (its side effects are visible).
{
    my @array = <a b c>;
    my $x = [ gather { while @array { take shift @array } } ];
    is-deeply $x, [<a b c>], 'gather inside [...] reifies its elements';
    is @array.elems, 0, 'gather body ran at array-build time (array drained)';
}

# Nested gather/take with an outer gather-while that depends on the inner
# gather draining the shared array (99problems "pack" idiom).
{
    sub group2 (*@array is copy) {
        gather while @array {
            take [
                gather {
                    my $h = @array[0];
                    while @array and $h eq @array[0] {
                        take shift @array;
                    }
                }
            ];
        }
    }
    is-deeply [group2(<a a a a b c c a a d e e e e>)],
        [[<a a a a>], [<b>], [<c c>], [<a a>], [<d>], [<e e e e>]],
        'nested gather-while + inner gather draining a shared array';
}

# Multi-element literals keep each gather itemized as its own element.
is-deeply [[gather { take 1; take 2 }], [gather { take 3 }]],
    [[1, 2], [3]], 'nested array-of-gathers keeps each itemized';

# A `lazy`-marked infinite gather must NOT be force-materialized by the array
# literal — that would never terminate. We only assert that construction
# terminates here; lazily slicing such a list back out of the array literal is a
# separate, deeper lazy-sequence feature mutsu does not yet support.
{
    my @a = [lazy gather { take $_ for 1..Inf }];
    pass 'lazy infinite gather in [...] does not hang at build time';
}
