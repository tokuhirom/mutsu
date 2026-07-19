use v6;
use Test;

# The `item()` FUNCTION form must itemize its argument the same way the `.item`
# METHOD form does: the result is a SINGLE non-flattening element in list
# context. Previously `item(1..3)` and `item(1,2,3)` flattened under `for`.

plan 11;

# Single non-array aggregate (Range): stays whole, iterated as one element.
{
    my @seen;
    @seen.push($_) for item(1..3);
    is @seen.elems, 1, 'item(Range) is a single element under for';
    is @seen[0].WHAT.^name, 'Range', 'the single element is the Range itself';
}

# Multiple args become a single itemized list.
{
    my @seen;
    @seen.push($_) for item(1, 2, 3);
    is @seen.elems, 1, 'item(1,2,3) is a single element under for';
    is @seen[0].elems, 3, 'the single element holds all three values';
}

# .WHAT is preserved (itemization does not change the type object).
is item(1..3).WHAT.^name, 'Range', 'item(Range).WHAT is Range';
is item(1, 2, 3).WHAT.^name, 'List', 'item(1,2,3).WHAT is List';
is item(42).WHAT.^name, 'Int', 'item(Int).WHAT is Int';

# Scalars still numify/stringify transparently through the item container.
is item(42) + 1, 43, 'item(Int) still participates in numeric ops';
is item("abc"), "abc", 'item(Str) stringifies to itself';

# A Hash stays a single element and keeps its identity.
{
    my %h = a => 1, b => 2;
    my @seen;
    @seen.push($_) for item(%h);
    is @seen.elems, 1, 'item(Hash) is a single element under for';
}

# Matches the .item method form exactly.
is-deeply item(1..3).WHAT, (1..3).item.WHAT, 'function and method item agree';
