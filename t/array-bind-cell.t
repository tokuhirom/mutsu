use v6;
use Test;

plan 20;

# Phase 2 Stage 2: array-element `:=` binds (`@x[i] := <source>`) install a
# shared ContainerRef cell at the element (no BOUND_ARRAY_REF_SENTINEL
# back-references). Both sides of the bind alias one container, so writes
# through either side are mutually visible, and `=:=` compares cell identity.

# Element source (array -> array): write-through both ways
{
    my @b = 1, 2, 3; my @x;
    @x[0] := @b[1];
    @b[1] = 99;
    is @x[0], 99, 'array-element source: source write visible through bound element';
    @x[0] = 42;
    is @b[1], 42, 'array-element source: bound-element write visible through source';
}

# Element source (hash -> array): write-through both ways
{
    my %src = a => 10; my @y;
    @y[0] := %src<a>;
    %src<a> = 20;
    is @y[0], 20, 'hash-element source: source write visible through bound element';
    @y[0] = 30;
    is %src<a>, 30, 'hash-element source: bound-element write visible through source';
}

# =:= identity on bound elements
{
    my $v = 5; my @z;
    @z[0] := $v;
    ok @z[0] =:= $v, 'scalar source: bound element =:= source';

    my @b = 1, 2;
    my @w = 3, 4;
    @w[0] := @b[0];
    ok @w[0] =:= @b[0], 'array-element source: bound elements share identity';
    nok @w[0] =:= @b[1], 'bound element not identical to a different element';
}

# Iteration / stringification sees the bound value (decont chokepoint)
{
    my @it; my $t = 1;
    @it[0] := $t;
    $t = 7;
    is @it.join(","), "7", 'join sees the current bound value';
    is ~@it, "7", 'stringification sees the current bound value';
    is @it.gist, "[7]", 'gist sees the current bound value';
}

# Array-valued source: push and deep write through the bound element are
# both visible — the bound element and the source alias one container.
{
    my @inner = 1, 2; my @holder;
    @holder[0] := @inner;
    @inner.push(3);
    is-deeply @holder[0], [1, 2, 3], 'array source: push through source visible';
    @holder[0][0] = 9;
    is @inner[0], 9, 'array source: deep write through bound element reaches source';
    @inner[1] = 5;
    is @holder[0][1], 5, 'array source: source write visible through bound element';
}

# Hash-valued source: deep write through the bound array element
{
    my %hsrc = a => 1, b => 2; my @h;
    @h[0] := %hsrc;
    @h[0]<a> = 99;
    is %hsrc<a>, 99, 'hash source: deep write through bound array element';
}

# Array-valued source bound to a hash key: deep write reaches the source
{
    my @arr = 10, 20; my %hold;
    %hold<k> := @arr;
    %hold<k>[1] = 88;
    is @arr[1], 88, 'array source via hash key: deep write reaches source';
}

# Two bound elements aliasing the same source: a deep write is visible
# through both (and through the source).
{
    my @src = 1, 2; my @x; my @y;
    @x[0] := @src;
    @y[0] := @src;
    @x[0][0] = 7;
    is @y[0][0], 7, 'shared source: deep write visible through sibling bound element';
    is @src[0], 7, 'shared source: deep write visible through source';
}

# Assignment snapshots: copying an array with a bound element deconts the cell
{
    my @orig; my $s = "a";
    @orig[0] := $s;
    $s = "b";
    my @copy = @orig;
    $s = "c";
    is @orig[0], "c", 'original array still aliases the source';
    is @copy[0], "b", 'assigned copy snapshot does not alias the source';
}

# Writing through a bound element after the source was reset is allowed
{
    my @a = 1, 2, 3; my @x;
    @x[0] := @a[1];
    @a = 9;            # array reset; binding metadata is now stale
    @x[0] = 5;
    is @x[0], 5, 'write to a bound element succeeds after source reset';
}

done-testing;
