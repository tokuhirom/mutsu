use Test;

plan 19;

# `.flat` un-itemizes its top-level receiver, then flattens (Raku semantics).
# A bare `$(...)`/`$[...]` was treated as a single opaque item before, so
# `.flat` wrongly returned one element.
is $(1, 2, 3).flat.elems, 3, '$(...) .flat un-itemizes the receiver';
is $[1, 2, 3].flat.elems, 3, '$[...] .flat un-itemizes the receiver';
is (1, 2, 3).flat.elems, 3, 'plain list .flat unchanged';
is $(1, (2, 3)).flat.elems, 3, 'nested list inside itemized receiver flattens';
is $((1, 2), (3, 4)).flat.elems, 4, 'itemized receiver of lists flattens recursively';
is-deeply $(1, 2, 3).flat.list, (1, 2, 3), '.flat content preserved';

# The `flat()` function de-itemizes its operands too.
is flat($(1, 2, 3)).elems, 3, 'flat() un-itemizes its argument';
is flat(1, [2, [3, 4]], (5, 6)).elems, 5, 'flat() keeps nested [...] (no over-flatten)';

# A scalar value flattens to itself.
is $42.flat.elems, 1, 'scalar .flat is a single element';

# Nested itemized items inside a real array stay single (NOT recursed).
{
    my @a = $[1, 2], $[3, 4];
    is @a.flat.elems, 2, 'array of itemized arrays keeps them single';
}

# A hash flattens to its pairs in list context.
{
    my %h = a => 1, b => 2;
    is %h.flat.elems, 2, 'bare hash .flat yields its pairs';
    is $(%h).flat.elems, 2, 'itemized hash .flat un-itemizes to pairs';
    is flat(%h).elems, 2, 'flat(%h) yields pairs';
}

# A hash nested inside a real array is itemized and stays a single element.
{
    my @aoh = { a => 1 }, { b => 2 };
    is @aoh.flat.elems, 2, 'array of hashes keeps each hash single';
    is @aoh.flat[0].WHAT.^name, 'Hash', 'nested hash stays a Hash (not flattened to pairs)';
    my %h = a => 1, b => 2;
    my @mixed = %h, %h;
    is @mixed.flat.elems, 2, 'array of multi-key hashes does not flatten to pairs';
}

# Only the TOP-LEVEL operand flattens. In a multi-arg `flat`, a (scalar-held)
# hash argument is one element of the list and stays a single hash — Raku's
# onearg rule: only the single-arg form un-itemizes its operand.
{
    my $hash = { a => 1, b => 2 };
    my @r = flat $hash, <a b c>;
    is @r.elems, 4, 'multi-arg flat keeps a hash argument single (onearg rule)';
    is @r[0].WHAT.^name, 'Hash', 'hash argument is not flattened to pairs in multi-arg flat';
    # But the single-arg / method forms DO flatten a (scalar-held) hash.
    is $hash.flat.elems, 2, 'scalar-held hash .flat yields pairs';
}
