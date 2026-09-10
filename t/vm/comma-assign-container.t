use Test;

plan 8;

# `,=` desugars to `$x = $x, RHS` and has list precedence (the whole RHS is the
# second operand). See roast S03-operators/assign.t (RT #76414).

# On a hash, `%h ,= %other` is `%h = %h, %other`, which flattens both hashes to
# pairs and re-hashes them: a merge.
{
    my %part1 = a => 'b';
    my %part2 = d => 'c';
    my %both = %part1, %part2;

    my %retval = ( %part1 ,= %part2 );
    ok %retval eqv %both, ',= works for hashes (return value)';
    ok %part1  eqv %both, ',= works for hashes (hash modified)';
}

# On an array, `@a ,= 3, 4` is `@a = @a, (3, 4)`: the array becomes a two-element
# list whose first element is the (now self-referential) array itself and whose
# second element is the itemized RHS list.
{
    my @a = 1, 2;
    @a ,= 3, 4;
    is @a.elems, 2, ',= on an array does NOT flatten-append';
    ok @a[0] =:= @a, ',= inserts the array itself as element 0 (self-reference)';
    is @a[1].join('|'), '3|4', ',= inserts the whole RHS list as element 1';
}

# statement vs expression context both work.
{
    my @a = 1, 2;
    my @r = (@a ,= 3);
    is @a.elems, 2, ',= expression context (array modified)';
    is @r.elems, 2, ',= expression context (return value)';
}

{
    my %h = x => 1;
    %h ,= (y => 2);
    ok %h eqv {x => 1, y => 2}, ',= statement context on a hash';
}
