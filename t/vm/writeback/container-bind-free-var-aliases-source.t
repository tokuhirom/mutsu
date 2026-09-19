use Test;

# A whole-container `:=` written INSIDE a named sub, against two outer-scope
# `@`/`%` names, must leave the two names on ONE container -- exactly as the
# same binding written at file scope does.
#
# It did not (#8759). Such a bind is routed through `OpCode::SetGlobal`, whose
# shared-`ContainerCell` branch excluded `@`/`%` alongside `&`; the bind fell
# through to `pending_alias_bind_names` instead, which becomes a one-shot value
# copy plus a bidirectional `local_bind_pairs` entry. A pair is a *scalar*
# propagation mechanism -- its consumers copy a value between two local slots
# on a WHOLE-variable store -- so an element store, which writes no slot at
# all, propagated nothing, and the second `Gc` handle the copy handed the
# target was detached by the first COW mutation. The two names then drifted
# apart permanently.
#
# All expectations below were measured against raku v2026.07.

plan 27;

# --- the issue's own shape -------------------------------------------------
my @src = 1, 2, 3;
my @dst;
sub bind-array() { @dst := @src }
bind-array();
is @dst.elems, 3, 'the bound name sees the source container';
@src[1] = 20;
is @dst[1], 20, 'an element store through the source is visible through the alias';
@dst[2] = 30;
is @src[2], 30, 'an element store through the alias is visible at the source';
is @src.join(','), '1,20,30', 'both stores landed in the one container';
is-deeply @dst.List, @src.List, 'alias and source still name one array';

# A mutating METHOD, not just an element store: the push has to find the Array
# inside the shared cell rather than dispatching on the cell itself.
@dst.push(4);
is @src.join(','), '1,20,30,4', 'a push through the alias reaches the source';
@src.push(5);
is @dst.join(','), '1,20,30,4,5', 'a push through the source reaches the alias';
ok @src =:= @dst, 'the two names are the same container';

# --- the `%` twin ----------------------------------------------------------
my %hs = a => 1;
my %hd;
sub bind-hash() { %hd := %hs }
bind-hash();
%hs<b> = 2;
is %hd<b>, 2, 'a key added through the source is visible through the alias';
%hd<c> = 3;
is %hs<c>, 3, 'a key added through the alias is visible at the source';
is %hs.elems, 3, 'the source hash has all three keys';
is %hd.elems, 3, 'and so does the alias';

# --- a typed source --------------------------------------------------------
# A bound `@` adopts the SOURCE container's element type, not its own.
my Int @typed = 1, 2, 3;
my @adopt;
sub bind-typed() { @adopt := @typed }
bind-typed();
is @adopt.of.^name, 'Int', 'the alias adopts the source element type';
is @adopt.elems, 3, 'and reads the source container, not a cell holding it';
dies-ok { @typed[0] = "x" }, 'the source still enforces its element type';

# A target with a declared type of its own adopts the SOURCE's, and enforces
# it under that name. (An UNDECLARED target -- `@adopt` above -- reports the
# adopted `.of` but does not yet enforce it under its own name: the element
# store consults the name-keyed constraint, which nothing registers for it.
# That predates this file and is unchanged by it.)
my Int @src-typed = 1, 2, 3;
my Cool @declared;
sub bind-declared() { @declared := @src-typed }
bind-declared();
is @declared.of.^name, 'Int', 'a declared target adopts the source element type';
dies-ok { @declared.push("x") }, 'and enforces the adopted type, not its own';

# --- an empty source still aliases ------------------------------------------
# Nothing to copy at bind time, so a snapshot-shaped implementation looks
# right here and diverges on the first mutation.
my @empty;
my @alias;
sub bind-empty() { @alias := @empty }
bind-empty();
@empty.push(7);
is @alias.elems, 1, 'the alias tracks a source that was empty at bind time';
is @alias.join(','), '7', 'and sees the value pushed after the bind';

# --- the scalar twin -------------------------------------------------------
# This shape already aliased, but every name read handed the container on
# where the contained value was wanted, so `.elems` counted the container
# (1) instead of the Array it held.
my $sp = [1, 2];
my $sq;
sub bind-scalar() { $sq := $sp }
bind-scalar();
is $sq.elems, 2, 'a scalar alias dispatches on the array it holds';
$sp = [9];
is $sq.raku, '$[9]', 'and follows a whole-value store through the source';

# --- a chain of three names -------------------------------------------------
my @c1 = 1;
my @c2;
my @c3;
sub bind-chain() { @c2 := @c1; @c3 := @c2 }
bind-chain();
@c1.push(2);
is @c3.join(','), '1,2', 'a third name bound to the second joins the same container';
ok @c1 =:= @c3, 'all three names are one container';

# --- the file-scope spelling is unchanged -----------------------------------
my @fs-src = 1, 2;
my @fs-dst := @fs-src;
@fs-src[0] = 9;
is @fs-dst[0], 9, 'the `SetLocal` twin still aliases';
@fs-dst.push(3);
is @fs-src.join(','), '9,2,3', 'and still shares one container under a push';

# --- a bind INTO a free variable must not reach an intervening caller -------
# The counterweight to everything above: sharing a cell must not leak the
# binding into an unrelated same-named lexical of a caller frame.
my @outer = 1;
my @target;
sub g-bind() { @target := @outer }
sub f-shadow() { my @outer = 5, 6; g-bind(); @outer.join(',') }
is f-shadow(), '5,6', 'the caller keeps its own same-named lexical';
is @target.join(','), '1', 'and the bind named the lexical it actually saw';
