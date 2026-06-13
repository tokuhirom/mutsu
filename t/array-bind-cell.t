use v6;
use Test;

plan 14;

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

# Array-valued source: push through the source is visible.
# (Deep write *through the bound element* into an array-valued source —
# `@holder[0][0] = 9` reaching `@inner[0]` — is a separate later slice: the
# whole-container source copies into the cell rather than aliasing.)
{
    my @inner = 1, 2; my @holder;
    @holder[0] := @inner;
    @inner.push(3);
    is-deeply @holder[0], [1, 2, 3], 'array source: push through source visible';
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
