use Test;
plan 25;

# map function form
{
    my @list = (1, 2, 3, 4, 5);
    my @result = map { $_ * 2 }, @list;
    is +@result, 5, 'map function form: element count';
    is @result.join(', '), '2, 4, 6, 8, 10', 'map function form: values';
}

# map method form with colon
{
    my @list = (1, 2, 3, 4, 5);
    my @result = @list.map: { $_ * 2 };
    is +@result, 5, 'map method colon form: element count';
    is @result.join(', '), '2, 4, 6, 8, 10', 'map method colon form: values';
}

# Range flattening in @-variable
{
    my @list = 1 .. 5;
    is +@list, 5, 'Range flattens into @-variable';
    is @list.join(', '), '1, 2, 3, 4, 5', 'Range elements are correct';
}

# map with Range
{
    my @result = (1..3).map: { $_ * 10 };
    is @result.join(', '), '10, 20, 30', 'map on Range works';
}

# slip in map
{
    my @list = (1, 2, 3);
    my @result = map { slip($_, $_ * 2) }, @list;
    is +@result, 6, 'slip flattens into map result';
    is @result.join(', '), '1, 2, 2, 4, 3, 6', 'slip values correct';
}

# .Slip method
{
    my @a = (1, 2, 3);
    my @result = map { ("v" => $_, "d" => $_ * 2).Slip }, @a;
    is +@result, 6, '.Slip flattens pairs into result';
}

# Multi-arity map (method form)
{
    is-deeply (1, 2, 3, 4).map({ $^a + $^b }), (3, 7),
        'map with 2-ary block';
}

# Multi-arity map (function form with pointed block)
{
    my @a = (1, 4, 2, 5, 3, 6);
    my @ret = map -> $a, $b { $a + $b }, @a;
    is @ret.elems, 3, 'pointed block map: took 2 at a time';
    is @ret[0], 5, 'pointed block map: first ok';
    is @ret[1], 7, 'pointed block map: second ok';
    is @ret[2], 9, 'pointed block map: third ok';
}

# map shouldn't flatten array objects
{
    my @foo = [1, 2, 3].map: { [100 + $_, 200 + $_] };
    is +@foo, 3, "map doesn't flatten arrayrefs (count)";
    is +@foo[0], 2, "map doesn't flatten arrayrefs (inner count)";
}

# map on single values
{
    is 42.map({ $_ }), 42, 'map on Int works';
    is 'str'.map({ $_ }), 'str', 'map on Str works';
}

# .() invocation in map
{
    is ~(({1}, {2}, {3}).map: { .() }), "1 2 3", '.() calls $_ in map';
}

# next in map
{
    is (1..4).map({ next if $_ % 2; 2 * $_ }).join('|'),
        '4|8', 'next in map skips elements';
}

# last in map
{
    is (1..10).map({ last if $_ % 5 == 0; 2 * $_ }).join(' '),
        '2 4 6 8', 'last in map exits early';
}

# empty return values in map
{
    my @array = <a b c d>;
    my @result = map { () }, @array;
    is +@result, 4, 'map with () body returns elements';
}

# multiple statements in map
{
    my @list = (1, 2, 3);
    my @result = map {
        my $x = "item/$_";
        $x;
    }, @list;
    is @result.join('|'), 'item/1|item/2|item/3', 'multiple statements in map';
}

# dies-ok for mismatched arity
{
    dies-ok { (1, 2, 3).map({ $^a + $^b }) },
        'odd-number list with 2-ary block dies';
}
