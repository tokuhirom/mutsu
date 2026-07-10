use v6;
use Test;

# Nested-subscript mutating methods (`%h<a><b>.push`) run with chained
# element-for-mutation loads and NO post-call writeback (container
# identity §3.2). All expectations verified against Rakudo.

plan 27;

# --- autoviv shapes: intermediate kind follows the NEXT subscript ---
{
    my %h; %h<a><b>.push(1,2);
    is-deeply %h<a><b>, [1,2], 'hash-hash autoviv push';
    my %g; %g<a>[1].push(9);
    ok %g<a> ~~ Positional, 'assoc-then-positional vivifies an Array';
    is-deeply %g<a>[1], [9], 'positional intermediate element pushed';
    ok !%g<a>[0].defined, 'hole before positional index stays undefined';
    my @a; @a[0]<x>.push(1);
    is-deeply @a[0]<x>, [1], 'array-hash autoviv push';
    my @b; @b[0][1].push(1);
    is-deeply @b[0][1], [1], 'array-array autoviv push';
    my %d; %d<a><b><c>.push(1);
    is-deeply %d<a><b><c>, [1], 'three-level autoviv push';
    my %e; %e<a>[0]<c>.push(5);
    is-deeply %e<a>[0]<c>, [5], 'mixed three-level autoviv push';
}

# --- existing nested elements mutate in place ---
{
    my %h = a => {b => [1,2,3]};
    %h<a><b>.push(4);
    is-deeply %h<a><b>, [1,2,3,4], 'push onto existing nested array';
    my %p = a => {b => [1,2,3]};
    my $r = %p<a><b>.pop;
    is $r, 3, 'pop returns the removed element';
    is-deeply %p<a><b>, [1,2], 'pop removes from the nested array (no writeback of the result)';
    my %s = a => {b => [1,2,3]};
    is %s<a><b>.shift, 1, 'shift returns the removed element';
    is-deeply %s<a><b>, [2,3], 'shift removes from the nested array';
    my %u = a => {b => [1,2]};
    %u<a><b>.unshift(0);
    is-deeply %u<a><b>, [0,1,2], 'unshift onto existing nested array';
    my %ap = a => {b => [1]};
    %ap<a><b>.append(2,3);
    is-deeply %ap<a><b>, [1,2,3], 'append onto existing nested array';
    my %sp = a => {b => [1,2,3,4]};
    %sp<a><b>.splice(1,2);
    is-deeply %sp<a><b>, [1,4], 'splice removes the slice in place (result not written back)';
}

# --- missing elements: pop/shift/splice die without growing anything ---
{
    my %h;
    try { %h<a><b>.pop };
    is-deeply %h, {}, 'pop on missing nested element does not autovivify';
    my %s;
    try { %s<a><b>.shift };
    is-deeply %s, {}, 'shift on missing nested element does not autovivify';
    my %sp;
    try { %sp<a><b>.splice(0,1) };
    is-deeply %sp, {}, 'splice on missing nested element does not autovivify';
}

# --- non-container elements raise without modifying ---
{
    my %h = a => {b => 5};
    try { %h<a><b>.push(1) };
    is %h<a><b>, 5, 'push onto non-container nested element leaves it unchanged';
    my %g = a => 5;
    try { %g<a><b>.push(1) };
    is %g<a>, 5, 'push through non-container intermediate leaves it unchanged';
}

# --- aliasing: reads share the mutated node ---
{
    my %h = a => {b => [1]};
    my $t = %h<a>;
    %h<a><b>.push(2);
    is-deeply $t<b>, [1,2], 'earlier read of the intermediate sees the push';
    my %g; my $r = %g<a><b>.push(1,2);
    is-deeply $r, [1,2], 'push returns the array';
}

# --- junction final key vivifies every addressed element ---
{
    my %h; %h<a>{"x"|"y"}.push(1);
    is-deeply %h<a><x>, [1], 'junction key vivifies x';
    is-deeply %h<a><y>, [1], 'junction key vivifies y';
}

# --- non-variable bases write through the shared node ---
{
    my %g = x => {};
    sub f() { %g }
    f()<x><y>.push(2);
    is-deeply %g<x><y>, [2], 'call-base nested push writes through';
    my %m;
    sub g() { %m }
    g()<zz><y>.push(3);
    is-deeply %m<zz><y>, [3], 'call-base nested push autovivifies the intermediate';
}

done-testing;
