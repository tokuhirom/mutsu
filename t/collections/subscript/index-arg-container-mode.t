use Test;

# ADR-0059 Slice 3: a single-dimension subscript passed to a NAMED routine
# hands the callee the element's own location (the `IndexArgRef` producer
# gated on the callee's signature), instead of the retired copy-in/copy-out
# `__mutsu_index_rw_arg_*` temps. Every expectation matches rakudo.

plan 41;

sub rw($x is rw) { $x = 9 }
sub raw(\x) { x = 8 }
sub rd($x) { $x }
sub rrd(\x) { x }
sub rc(\x) { x.VAR.^name }

{
    my @a = 1, 2;
    rw(@a[0]);
    is @a.raku, '[9, 2]', 'is rw writes an existing element';
}
{
    my @a = 1, 2;
    rw(@a[5]);
    is @a.raku, '[1, 2, Any, Any, Any, 9]', 'a write past the end grows the array';
}
{
    my %h;
    rw(%h<k>);
    is-deeply %h, {k => 9}, 'a write to a missing key creates it';
}
{
    my %h;
    raw(%h<a>);
    is-deeply %h, {a => 8}, 'a sigilless parameter writes a missing key too';
}

# Read-safety: binding the location creates nothing; only a write does.
{
    my @a = 1, 2;
    rd(@a[5]);
    rrd(@a[5]);
    is @a.elems, 2, 'reading past the end does not grow the array';
    my %h;
    rd(%h<z>);
    rrd(%h<z>);
    is %h.elems, 0, 'reading a missing key does not create it';
}
{
    my @a;
    is rc(@a[0]), 'Scalar', 'a sigilless parameter sees a Scalar container';
    is @a.elems, 0, '... without vivifying the element';
    my %h;
    is rc(%h<q>), 'Scalar', 'the associative twin sees a Scalar too';
    is %h.elems, 0, '... without creating the key';
}

# The index expression is evaluated once (the temps re-evaluated it for the
# writeback, so a side-effecting index wrote to the wrong slot).
{
    my @a = 1, 2;
    my $i = 0;
    rw(@a[$i++]);
    is @a.raku, '[9, 2]', 'the element the index named is written';
    is $i, 1, 'the index expression ran once';
}

# An argument after a `|slip` has no compile-time parameter index.
{
    my @c = 1;
    my @a = 1, 2;
    sub two($p, $q is rw) { $q = 5 }
    two(|@c, @a[1]);
    is @a.raku, '[1, 5]', 'an argument after a slip still binds its container';
}

# A routine declared after its use site, a lexical `&` callable and a
# lexical sub are all answered at run time.
{
    my @a = 1, 2;
    fwd(@a[0]);
    sub fwd($x is rw) { $x = 11 }
    is @a.raku, '[11, 2]', 'a routine declared later';
}
{
    my @a = 1, 2;
    my &r = sub ($x is rw) { $x = 3 };
    r(@a[0]);
    is @a.raku, '[3, 2]', 'a lexical & callable called by name';
}
{
    my @a = 1, 2;
    my sub ls($x is rw) { $x = 4 };
    ls(@a[1]);
    is @a.raku, '[1, 4]', 'a lexical sub';
}

# A multi is answered over its candidates; a non-rw candidate is unaffected.
{
    multi m(Int $x is rw) { $x = 7 }
    multi m(Str $x) { $x }
    my @a = 1, 2;
    m(@a[0]);
    is @a.raku, '[7, 2]', 'the rw multi candidate writes';
    my @s = <x y>;
    is m(@s[0]), 'x', 'the read-only candidate reads';
}

# A user-defined infix operator with an `is rw` operand.
{
    sub infix:<pe>($a is rw, $b) { $a += $b }
    my @a = 3, 4;
    @a[1] pe 5;
    is @a.raku, '[3, 9]', 'an infix operator writes a subscript operand';
}

# An expression-level bind of a subscript aliases the element.
{
    my @a = 3, 4;
    my $x;
    ($x := @a[0]);
    $x = 7;
    is @a.raku, '[7, 4]', '($x := @a[0]) aliases the element';
}

# The same call site in a loop writes each iteration's own element.
{
    my @a = 1, 2, 3;
    rw(@a[$_]) for 0, 2;
    is @a.raku, '[9, 2, 9]', 'a looped call site writes each element';
}

# A NESTED subscript: a missing intermediate level has no location of its
# own, so the argument is compiled in container mode when the callee binds it
# (roast S02-types/autovivification.t).
{
    my %h;
    rd(%h<a><b>);
    rrd(%h<a><b>);
    is %h.elems, 0, 'reading a nested missing path creates nothing';
    rw(%h<a><b>);
    is-deeply %h, {a => {b => 9}}, 'writing it creates the whole path';
    my @a;
    rrd(@a[1][2]);
    is @a.elems, 0, 'reading a nested path past the end grows nothing';
    rw(@a[1][2]);
    is @a.raku, '[Any, [Any, Any, 9]]', 'writing it grows each level';
}
{
    my @a;
    my $x := @a[1][2];
    is @a.elems, 0, 'a nested past-the-end bind grows nothing';
}

# A missing key reads as the hash's default through its deferred token.
{
    my %h is default(42);
    is rrd(%h<a>), 42, 'is default is honoured for a missing key';
    my Int %t;
    is rrd(%t<a>).raku, 'Int', 'so is the value type';
}

# A nested subscript whose index is COMPUTED (#10044): the index expression
# runs once, and a missing intermediate level is still created on write.
{
    my %h;
    my @k = <x y>;
    my $i = 0;
    rw(%h{@k[$i++]}{"z"});
    is-deeply %h, {x => {z => 9}}, 'a computed intermediate key is created on write';
    is $i, 1, '... and its index expression ran once';
    rrd(%h{@k[1]}{@k[0]});
    is %h.elems, 1, 'reading through a computed missing key creates nothing';
    my @m;
    rw(@m[$i + 1][$i]);
    is @m.raku, '[Any, Any, [Any, 9]]', 'computed positional indices grow each level';
}

# A computed level that turns out to select several elements at run time is
# an ordinary read, not a deferred location.
{
    my @alpha = 'a' .. 'z';
    my $res := (1, 2, 3, 4).map({ $_ }).cache;
    is rrd(@alpha[$res[*]][0 .. *-2]).join, 'bcd', 'a whatever slice of a slice';
    my @n = [1, 2], [3, 4];
    is rrd(@n[*][0]).raku, '$[1, 2]', 'a * level';
    is rrd(@n[1][*-1]), 4, 'a WhateverCode level';
    my %g = a => {b => 1}, c => {b => 2};
    is-deeply rrd(%g<a c>[1]), {b => 2}, 'a key slice level';
    ok rrd(%g{'a' | 'c'}<b>) == 1 & 2, 'a junction key level';
    my %e;
    is rrd(%e<nope>{<a b>}).raku, '(Any, Any)', 'a slice below a missing key';
    is %e.elems, 0, '... creates nothing';
}

# The container-mode gate's own Bool is not left under the argument: a block
# whose value is the call answers the call's result.
{
    my @n = [1, 2], [3, 4];
    is (1,).map({ rd(@n[1][0]) }).raku, '(3,).Seq', 'a block ending in the call';
    is (1,).map({ slip(@n[1][0]) }).raku, '(3,).Seq', '... with a builtin callee';
}
