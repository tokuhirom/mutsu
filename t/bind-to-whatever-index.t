# Binding (`:=`) to a WhateverCode subscript (`@a[*-1] := 42`) is illegal — the
# index is a computed slice, not a fixed container slot — so rakudo throws
# X::Bind::Slice ("Cannot bind to Array slice"). A plain slice bind and a
# variable-index bind stay valid.
use Test;

plan 11;

throws-like { my @a; @a[*-1] := 42 }, X::Bind::Slice,
    'binding [*-1] of an empty array throws X::Bind::Slice';
throws-like { my @a = 1, 2, 3; @a[*-1] := 42 }, X::Bind::Slice,
    'binding [*-1] of a non-empty array throws X::Bind::Slice';
throws-like { my @a = 1, 2, 3; @a[*-2] := 42 }, X::Bind::Slice,
    'binding [*-2] throws X::Bind::Slice';

# The exception is a real type, carrying the attributes rakudo's does. It is
# NOT a subtype of X::Bind despite the name — its only parent is Exception.
{
    my $e = (try { my @a; @a[*-1] := 42 }, $!).tail;
    is $e.message, 'Cannot bind to Array slice', 'the message names the container type';
    is $e.type.^name, 'Array', '.type is the sliced container type';
    nok $e ~~ X::Bind, 'X::Bind::Slice does not inherit X::Bind';
    isa-ok X::Bind::Slice.new(type => Array), X::Bind::Slice,
        'and the class can be constructed directly';
}

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
