use Test;

plan 6;

# `,=` on a hash merges the right-hand hash into the left (like `%h = %h, %other`).
{
    my %part1 = a => 'b';
    my %part2 = d => 'c';
    my %both = %part1, %part2;

    my %retval = ( %part1 ,= %part2 );
    ok %retval eqv %both, ',= works for hashes (return value)';
    ok %part1  eqv %both, ',= works for hashes (hash modified)';
}

# `,=` on an array appends (like push).
{
    my @a = 1, 2;
    @a ,= 3, 4;
    is @a.join('|'), '1|2|3|4', ',= on an array appends';
}

# statement vs expression context both work.
{
    my @a = 1, 2;
    my @r = (@a ,= 3);
    is @a.join('|'), '1|2|3', ',= expression context (array modified)';
    is @r.join('|'), '1|2|3', ',= expression context (return value)';
}

{
    my %h = x => 1;
    %h ,= (y => 2);
    ok %h eqv {x => 1, y => 2}, ',= statement context on a hash';
}
