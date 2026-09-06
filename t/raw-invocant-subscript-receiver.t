use v6.e.PREVIEW;
use MONKEY-TYPING;
use Test;

# ADR-0067's subscript-receiver producer: `@a[0].mut` / `%h<a>.mut`, where `mut`
# declares a raw invocant, must mutate the ELEMENT.
#
# Slice 3b made the arrival direction work for a receiver the call site can
# name; a subscript receiver has no name, and `Index` has already read the
# element's value onto the stack by the time `CallMethod` runs. So this is a
# producer: `IndexInvocantRef` hands over the element's own `Scalar` cell, and
# `CallMethod`'s pre-existing decontainerize chokepoint keeps that invisible to
# every callee that does not bind its invocant raw.
#
# The emission is unconditional (rawness depends on the element's RUNTIME type),
# so the regression half of this file is the load-bearing half: with the
# program-wide raw-invocant flag raised by the `augment` below, every ordinary
# method call over a subscript receiver in this file is running through the
# producer's gate.

plan 33;

augment class Int {
    method mut(\S:) { S = 7 }
    method mutraw($s is raw:) { $s = 8 }
    method mutrw($s is rw:) { $s = 9 }
    method mutarg(\S: $v) { S = $v }
    method plain($s:) { $s + 1 }
}
augment class Str { method smut(\S:) { S = "X" } }

# --- the two rows the ADR named as acceptance (I3 / K3) --------------------
{
    my @a = 1, 2;
    @a[0].mut;
    is @a.raku, '[7, 2]', 'a positional subscript receiver hands over its element';
}
{
    my %h = a => 1;
    %h<a>.mut;
    is %h<a>, 7, 'an associative subscript receiver hands over its element';
}

# --- all three raw-invocant spellings, over both binders -------------------
{
    my @a = 1, 2;
    @a[0].mutraw;
    is @a[0], 8, '$s is raw: over a subscript receiver (slow binder)';
}
{
    my @a = 1, 2;
    @a[0].mutrw;
    is @a[0], 9, '$s is rw: over a subscript receiver (slow binder)';
}
{
    my @a = 1, 2;
    @a[1].mutarg(5);
    is @a.raku, '[1, 5]', 'an argument-carrying raw-invocant method';
}
{
    my @s = "a", "b";
    @s[1].smut;
    is @s.raku, '["a", "X"]', 'a Str element through an augmented native class';
}

# --- the shapes a subscript can take --------------------------------------
{
    my @a = 1, 2, 3;
    @a[*-1].mut;
    is @a.raku, '[1, 2, 7]', 'a WhateverCode subscript addresses one element';
}
{
    my @a = 1, 2, 3;
    my $i = 1;
    @a[$i + 1].mut;
    is @a.raku, '[1, 2, 7]', 'a computed subscript';
}
{
    my $calls = 0;
    my sub idx() { $calls++; 0 }
    my @a = 1, 2;
    @a[idx()].mut;
    is "{@a.raku} {$calls}", '[7, 2] 1', 'the subscript expression is evaluated exactly once';
}
{
    my @a = [1, 2], [3, 4];
    @a[0][1].mut;
    is @a.raku, '[[1, 7], [3, 4]]', 'a nested positional subscript';
}
{
    my %h = a => [1, 2];
    %h<a>[0].mut;
    is %h<a>.raku, '$[7, 2]', 'a positional subscript under an associative one';
}
{
    my %h = a => { b => 1 };
    %h<a><b>.mut;
    is %h<a><b>, 7, 'a nested associative subscript';
}
{
    my @a = 1, 2;
    my $n = "mut";
    @a[0]."$n"();
    is @a.raku, '[7, 2]', 'the runtime method-name spelling';
}
{
    my @a[3] = 1, 2, 3;
    @a[1].mut;
    is @a[1], 7, 'a shaped array element';
}
{
    my Int @a = 1, 2;
    @a[0].mut;
    is @a.raku, 'Array[Int].new(7, 2)', 'a typed array element';
}
{
    class R { has $.n is rw }
    my $r = R.new(n => [1, 2]);
    $r.n[0].mut;
    # `.raku` is deliberately not asserted here: a `$`-attribute holding an
    # Array renders itemized in raku (`$[7, 2]`) and unitemized in mutsu,
    # which is an unrelated divergence.
    is $r.n[0], 7, 'an attribute-held array element';
}
{
    role Bumpy { method bump(\S:) { S = 99 } }
    my @a = 1, 2;
    @a[0].&(Bumpy.^lookup('bump'));
    is @a[0], 99, 'a role-composed raw-invocant method';
}
{
    my @a = 1, 2;
    @a[0].mut;
    @a[0].mutarg(11);
    is @a.raku, '[11, 2]', 'the same element mutated twice keeps one container';
}
{
    my @a = 0 xx 200;
    for ^200 -> $i { @a[$i].mutarg($i) }
    is "{@a[0]} {@a[99]} {@a[199]} {@a.elems}", '0 99 199 200',
        'a loop past the JIT threshold mutates every element';
}

# --- controls: shapes with no element location to hand over ----------------
{
    my @a = 1, 2;
    @a[0].plain;
    is @a.raku, '[1, 2]', 'a non-raw invocant must not write through (control)';
}
{
    my @a = 1, 2;
    is @a[0].plain, 2, 'a non-raw invocant still returns its value (control)';
}
{
    my @a = 1, 2;
    dies-ok { @a[5].mut }, 'past the end there is no element to hand over';
    is @a.elems, 2, 'and the array did not grow';
}
{
    my %h = a => 1;
    dies-ok { %h<zz>.mut }, 'a missing key hands over no element';
}
{
    my @a = 1, 2, 3;
    dies-ok { @a[0, 1].mut }, 'a slice receiver is the slice, not an element';
}

# --- regression: ordinary method dispatch over a subscript receiver --------
# All of these run with the raw-invocant program-wide flag raised, so they are
# exercising the producer's decline path and the chokepoint that hides the
# container from an ordinary callee.
{
    my @a = 1, 2, 3;
    is "{@a[0].succ} {@a[1].WHAT.^name} {@a[2].Str} {@a[0] + @a[1]}", '2 Int 3 3',
        'value semantics over a positional subscript receiver';
    my %h = a => "x";
    is "{%h<a>.uc} {%h<a>.chars}", 'X 1',
        'value semantics over an associative subscript receiver';
}
{
    my @a = [1, 2], [3];
    @a[0].push(9);
    @a[1].append(4, 5);
    is @a.raku, '[[1, 2, 9], [3, 4, 5]]', 'in-place mutation of a container element';
    my %h = a => [1];
    %h<a>.push(2);
    is %h<a>.raku, '$[1, 2]', 'in-place mutation of a container hash value';
}
{
    my @a = 1, 2, 3;
    @a[0].mut;
    is "{@a.raku} {@a.gist} {@a.Str}", '[7, 2, 3] [7 2 3] 7 2 3',
        'a promoted element renders as its value';
    is "{@a.sort.join(',')} {@a.grep(* > 2).join(',')} {@a.sum}", '2,3,7 7,3 12',
        'a promoted element takes part in list operations as a value';
}
{
    my @a = 1, 2;
    my @b = @a;
    @a[0].mut;
    is "{@a.raku} {@b.raku}", '[7, 2] [1, 2]', 'promotion does not alias a copy';
}
{
    my %h = a => 1, b => 2;
    %h<a>.mut;
    my %g = %h;
    %g<a> = 5;
    is "{%h<a>} {%g<a>}", '7 5', 'promotion does not alias a copied hash';
}
