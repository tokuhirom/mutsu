use Test;

# Whole-container assignment (`@a = ...`, `%h = ...`) mutates the EXISTING
# container in place, preserving its identity so a by-value holder of the same
# container (an `@a` captured into a list) observes the reassignment — Raku's
# stable container identity. At the same time `=` still COPIES (`my @b = @a`
# yields a distinct container). See roast/S32-array/splice.t / BLOCKERS §3.
#
# NB: each block uses distinct variable names — a variable captured by a closure
# anywhere in the compilation unit switches to a shared-cell representation whose
# by-value snapshot semantics are a separate, still-open sub-case.

plan 15;

# --- array reassignment is observed through a captured reference ---
{
    my @arr = 0..9;
    my $cap = (0, @arr);
    ok $cap[1] =:= @arr, 'list element shares @arr container identity';
    @arr = ^5;
    is $cap[1].join(','), '0,1,2,3,4', 'reassignment visible through captured ref';
    ok $cap[1] =:= @arr, 'identity preserved across whole-array reassignment';
    @arr = 7, 8;
    is $cap[1].join(','), '7,8', 'second reassignment also visible';
}

# --- but `=` still copies: `my @cpy = @src` is a distinct container ---
{
    my @src = 1, 2, 3;
    my @cpy = @src;
    nok @cpy =:= @src, 'my @cpy = @src produces a distinct container';
    @cpy = 7, 8, 9;
    is @src.join(','), '1,2,3', '@src unaffected by later @cpy reassignment (copy)';
    is @cpy.join(','), '7,8,9', '@cpy holds its own values';
}

# --- hashes behave the same way ---
{
    my %tbl = a => 1, b => 2;
    my $cap = (0, %tbl);
    %tbl = c => 3;
    is $cap[1]<c>, 3, 'hash reassignment visible through captured ref';
    ok $cap[1] =:= %tbl, 'hash identity preserved across reassignment';

    my %orig = x => 1;
    my %dup = %orig;
    nok %dup =:= %orig, 'my %dup = %orig produces a distinct hash';
    %dup = y => 2;
    is %orig<x>, 1, '%orig unaffected by later %dup reassignment (copy)';
}

# --- self-referential comma-assignment still works ---
{
    my @sr = 1, 2;
    @sr ,= 3, 4;
    is @sr.elems, 2, ',= does not flatten-append';
    ok @sr[0] =:= @sr, ',= self-reference (element 0 is the array itself)';
}

# --- anonymous containers stay distinct (shared slot name) ---
{
    my @anon = (my % is default(42)), (my % is default(42) = o => 768);
    is @anon[0]<o>, 42, 'first anonymous hash keeps its own (default) value';
    is @anon[1]<o>, 768, 'second anonymous hash keeps its own value';
}
