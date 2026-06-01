use Test;

plan 6;

# Mutating an element of the topic ($_[idx] = ...) inside a `for @arr`
# loop must update each element in place and must NOT replace the whole
# source array with the last iteration's topic value.

{
    my @arr = [1], [2], [3];
    for @arr { $_[1] = 99; }
    is-deeply @arr, [[1, 99], [2, 99], [3, 99]],
        'for @arr { $_[1] = ... } mutates each element in place';
}

{
    my @arr = [1], [2], [3];
    $_[1] = 99 for @arr;
    is-deeply @arr, [[1, 99], [2, 99], [3, 99]],
        'statement-modifier for with $_ index mutation';
}

{
    # Mutating values obtained via %h.values in a for loop should update
    # the underlying hash, not corrupt the temporary array variable.
    my %h = (a => [1], b => [2]);
    my @v = %h.values;
    for @v { $_[1] = 99; }
    is-deeply %h, {a => [1, 99], b => [2, 99]},
        'for over %h.values copy mutates underlying arrays';
    is +@v.elems, 2, 'temporary @v keeps its element count';
}

{
    # The classify.t pattern: $_[1] = $_[0] for %result.values
    my %result := :{ 5 => [1], 10 => [2], 15 => [3], 20 => [4] };
    $_[1] = $_[0] for %result.values;
    is-deeply %result,
        :{ 5 => [1, 1], 10 => [2, 2], 15 => [3, 3], 20 => [4, 4] },
        'classify-style index writeback into object hash values';
    is +%result.keys, 4, 'object hash keeps all keys after mutation';
}
