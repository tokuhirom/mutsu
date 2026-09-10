use Test;

# `.^parameterize` must curry the base QuantHash type each time, never the
# previous curried spelling.  The native constructor then owns key coercion
# and preserves the requested `.keyof` type.
my @types = Set, Bag, Mix, SetHash, BagHash, MixHash;

plan 45;

for @types -> \T {
    my $type := T.^parameterize(Str);
    is-deeply $type.new(<a b>).keys.sort.List, <a b>, 'Str parameterization constructs';
    ok $type.keyof =:= Str, 'Str parameterization reports keyof';

    $type := T.^parameterize(Int());
    is-deeply $type.new(<1 2>).keys.sort.List, (1, 2), 'Int() parameterization coerces keys';
    ok $type.keyof =:= Int(), 'Int() parameterization reports keyof';

    $type := T.^parameterize(Date());
    is $type.new('2026-05-05').keys.head, '2026-05-05'.Date, 'Date() parameterization coerces keys';
    ok $type.keyof =:= Date(), 'Date() parameterization reports keyof';
}

{
    my $type := Set.^parameterize(Str);
    $type := $type.^parameterize(Int());
    is-deeply $type.new(<1 2>).keys.sort.List, (1, 2), 're-parameterization starts from Set, not Set[Str]';
}

for SetHash, BagHash, MixHash -> \T {
    my $type := T.^parameterize(Int());
    my %qh := $type.new;
    %qh<42 666> = 1, 1;
    %qh.values.map({ $_ = 3 });
    is %qh.elems, 2, 'values map writes a nonzero weight through';
    %qh.values.map({ $_ = 0 });
    is %qh.elems, 0, 'values map removes zero weights';
}

{
    my %hash = a => 1, b => 2;
    %hash.values.map({ $_ = 3 });
    is-deeply %hash.values.sort.List, (3, 3), 'plain Hash values map writes through its element cells';
    %hash.values.map({ $_ = 0 });
    is-deeply %hash.values.sort.List, (0, 0), 'plain Hash values map retains ordinary zero values';
}
