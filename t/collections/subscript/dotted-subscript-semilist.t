use v6;
use Test;

# A subscript holds a SEMILIST, so `.[$i; $j]` is a multi-dimensional index
# exactly as the undotted `@a[$i; $j]` is. The dotted postfix spelling went
# through the wrapper that FLATTENS the dimensions into one `ArrayLiteral` for
# callers that cannot represent them, so `$c.[0; 1]` silently became the slice
# `$c[(0, 1)]` and answered with the whole container instead of the element --
# a wrong value, not a parse error. `%h.{"a"; "b"}` had the same shape.
#
# `.<>`, the dotted decontainerizing zen angle subscript, was missing outright:
# the `.<key>` arm needs a non-empty key, so the empty `<>` fell through every
# dotted postfix to a bare "Confused". `Game::Entities` writes
# `.[COMPONENTS; $i].<>`, which needs both halves (#8155).
#
# Every expectation below was measured against rakudo.

plan 21;

my @m = [[1, 2, 3], [4, 5, 6]];

# The two spellings of a multi-dimensional index now agree.
is @m.[0; 1], 2, '.[$i; $j] indexes the second dimension';
is @m.[1; 2], 6, '.[$i; $j] again, other corner';
is @m[0; 1],  2, 'the bracket spelling is unchanged';
is @m.[0; 1], @m[0; 1], 'dotted and bracket subscripts agree';

# Three dimensions, so the loop is exercised past one `;`.
{
    my @d = [[[7, 8], [9, 10]], [[11, 12], [13, 14]]];
    is @d.[0; 1; 0], 9,  '.[$i; $j; $k] indexes three dimensions';
    is @d.[1; 0; 1], 12, 'three dimensions again';
}

# A single dimension still lowers to a plain one-dimensional subscript, and a
# comma list is still a slice rather than a second dimension.
is @m.[0].gist, '[1 2 3]', '.[$i] is still a plain index';
is @m.[0, 1].gist, '([1 2 3] [4 5 6])', '.[$i, $j] is still a slice, not two dimensions';

# A trailing `;` terminates the last dimension rather than opening an empty one.
is @m.[0; 1;], 2, 'a trailing semicolon does not add a dimension';

# The associative dotted subscript takes a semilist too.
{
    my %h = a => { b => { c => 42 } };
    is %h.{"a"; "b"}.gist, '({c => 42})', '.{...; ...} indexes the nested hash';
    is %h.{"a"; "b"; "c"}.gist, '(42)', 'three associative dimensions';
    is %h{"a"; "b"; "c"}.gist, '(42)', 'the brace spelling is unchanged';
}

# It is an lvalue, like the bracket form.
{
    my @w = [[1, 2], [3, 4]];
    @w.[0; 1] = 99;
    is @w.gist, '[[1 99] [3 4]]', '.[$i; $j] assigns through to the element';
    @w[1; 0] = 55;
    is @w.gist, '[[1 99] [55 4]]', 'the bracket spelling still assigns';
}

# `:exists` still reaches the adverb handling.
{
    my $s = [[1, 2], [3, 4]];
    ok $s.[0; 1]:exists, '.[$i; $j]:exists';
}

# `.<>` is the dotted decontainerizing zen angle subscript.
{
    my $x = 5;
    is $x.<>, 5, '.<> on a scalar yields the value';
    my @a = 1, 2;
    is @a.<>.gist, '[1 2]', '.<> on an array yields the array';
    my %h = a => 1;
    is %h.<>.gist, '{a => 1}', '.<> on a hash yields the hash';
    my $c = [[1, 2], [3, 4]];
    is $c.[0].<>.gist, '[1 2]', '.<> chains after a dotted subscript';
}

# The distribution's own spelling: a semilist subscript with `.<>` chained onto
# it. Before the fix this was a hard parse error, which is what made
# `Game::Entities` unloadable rather than merely wrong.
{
    my $c = [[1, 2], [3, 4]];
    is $c.[0; 1].<>, 2, '.[$i; $j].<> parses and indexes (the Game::Entities shape)';
    is @m.[0; 2].<>, 3, 'and again on an array variable';
}

done-testing;
