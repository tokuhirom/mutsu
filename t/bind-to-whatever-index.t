# Binding (`:=`) to a WhateverCode subscript (`@a[*-1] := 42`) is illegal — the
# index is a computed slice, not a fixed container slot — so rakudo throws
# X::Bind::Slice ("Cannot bind to Array slice"). A plain slice bind and a
# variable-index bind stay valid.
use Test;

plan 7;

throws-like { my @a; @a[*-1] := 42 }, X::Bind::Slice,
    'binding [*-1] of an empty array throws X::Bind::Slice';
throws-like { my @a = 1, 2, 3; @a[*-1] := 42 }, X::Bind::Slice,
    'binding [*-1] of a non-empty array throws X::Bind::Slice';
throws-like { my @a = 1, 2, 3; @a[*-2] := 42 }, X::Bind::Slice,
    'binding [*-2] throws X::Bind::Slice';

# Valid binds are unaffected.
{
    my @a = 1, 2, 3;
    @a[0, 1] := 4, 5;
    is ~@a, '4 5 3', 'a slice bind (@a[0,1] := ...) stays valid';
}
{
    my @a = 1, 2, 3;
    my $i = 1;
    @a[$i] := 42;
    is ~@a, '1 42 3', 'a variable-index bind (@a[$i] := ...) stays valid';
}
{
    # A single concrete-index bind shares a container cell (element aliasing).
    my @a = 1, 2, 3;
    my $x = 9;
    @a[0] := $x;
    $x = 7;
    is @a[0], 7, 'a single concrete-index bind aliases the container';
    @a[0] = 5;
    is $x, 5, '...and writes through both ways';
}
