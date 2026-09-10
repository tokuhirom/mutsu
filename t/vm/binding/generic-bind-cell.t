# `:=` bind to a stack-computed element target (IndexAssignGeneric) installs a
# shared `ContainerRef` cell so both sides alias the same container. Before the
# Phase 2 Stage 2 cell conversion (SlotRef removal slice 1) the computed-target
# bind installed a stale `HashSlotRef`/`ArraySlotRef` by-reference into the env
# and the alias never propagated.
use Test;

plan 10;

# --- forward direction: write source -> element sees it ---
{
    my %store;
    sub getstore() { %store }
    my $s = 10;
    getstore()<a> := $s;
    $s = 99;
    is %store<a>, 99, 'hash computed-target bind: write source propagates to element';
}

{
    my @arr;
    sub getarr() { @arr }
    my $s = 5;
    getarr()[0] := $s;
    $s = 77;
    is @arr[0], 77, 'array computed-target bind: write source propagates to element';
}

# --- reverse direction: write element -> source sees it ---
{
    my %store;
    sub getstore() { %store }
    my $s = 10;
    getstore()<a> := $s;
    %store<a> = 5;
    is $s, 5, 'hash computed-target bind: write element propagates to source';
}

{
    my @arr;
    sub getarr() { @arr }
    my $t = 1;
    getarr()[0] := $t;
    @arr[0] = 9;
    is $t, 9, 'array computed-target bind: write element propagates to source';
}

# --- read direction: element reflects source's current value at bind ---
{
    my %r;
    sub gr() { %r }
    my $v = 42;
    gr()<z> := $v;
    is %r<z>, 42, 'computed-target bind: element reads source value';
}

# --- cell sharing: one source bound to two computed targets ---
{
    my @a1;
    my %h1;
    sub ga() { @a1 }
    sub gh() { %h1 }
    my $x = 7;
    ga()[0] := $x;
    gh()<k> := $x;
    $x = 100;
    is @a1[0], 100, 'shared source: array alias sees write';
    is %h1<k>, 100, 'shared source: hash alias sees write';
}

# --- deref-target bind (named-var deref, already cell path) still works ---
{
    my @raw = (1, 2, 3);
    my $ref = @raw;
    my $s = 8;
    $ref[1] := $s;
    $s = 42;
    is @raw[1], 42, 'deref-target bind aliases the underlying array';
}

# --- non-bind plain assignment to a computed target is unaffected ---
{
    my %h;
    sub gh2() { %h }
    gh2()<plain> = 11;
    is %h<plain>, 11, 'plain assignment to computed hash target unaffected';
}

{
    my @a;
    sub ga2() { @a }
    ga2()[2] = 33;
    is @a[2], 33, 'plain assignment to computed array target unaffected';
}
