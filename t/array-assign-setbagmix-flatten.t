use Test;

# Assigning a Set/Bag/Mix directly to an `@` variable flattens it into its
# `key => weight` pairs in list context, exactly like a Hash (Rakudo). A Set/
# Bag/Mix held in a `$` scalar stays a single item, like any scalar container.

plan 16;

# Direct assignment flattens into pairs
{
    my @a = set(1, 2, 3);
    is @a.elems, 3, 'Set assigned to @ flattens to 3 pairs';
    ok @a.all ~~ Pair, 'elements are Pairs';
    is @a.map(*.value).unique.list, (True,), 'Set pair values are True';
}
{
    my @a = bag(1, 1, 2);
    is @a.elems, 2, 'Bag assigned to @ flattens to 2 pairs';
    is @a.sort(*.key).map(*.value).join(','), '2,1', 'Bag weights preserved';
}
{
    my @a = mix('a' => 1.5, 'b' => 2);
    is @a.elems, 2, 'Mix assigned to @ flattens to 2 pairs';
}
{
    my %h = a => 1, b => 2;
    my @a = %h;
    is @a.elems, 2, 'Hash assigned to @ still flattens (regression guard)';
}

# Coercion methods stay consistent
{
    is set(1, 2, 3).Array.elems, 3, '.Array on a Set flattens';
    is set(1, 2, 3).list.elems, 3, '.list on a Set flattens';
}

# A Set/Bag/Mix in a `$` scalar stays a single item when assigned to @
{
    my $s = set(1, 2, 3);
    my @a = $s;
    is @a.elems, 1, 'scalar-held Set stays a single item';
    isa-ok @a[0], Set, 'the single item is the Set itself';
}
{
    my $h = %(a => 1, b => 2);
    my @a = $h;
    is @a.elems, 1, 'scalar-held Hash stays a single item';
    isa-ok @a[0], Hash, 'the single item is the Hash itself';
}
{
    my $b = bag(1, 1, 2);
    my @a = $b;
    is @a.elems, 1, 'scalar-held Bag stays a single item';
}

# An array literal does NOT flatten a Set (separate path)
{
    my @a = [set(1, 2, 3)];
    is @a.elems, 1, 'array literal [set(...)] stays a single element';
}

# A Set nested in a list stays one element
{
    my @a = (set(1, 2), 3);
    is @a.elems, 2, 'Set inside a list literal is one element';
}
