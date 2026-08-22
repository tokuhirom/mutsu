use v6;
use Test;

# ADR-0049: `Nil` decays to the *container's* default at the element store,
# and stops being a hole sentinel (docs/adr/0049-nil-decays-to-the-container-
# default-at-the-element-store.md).
#
# This file is the acceptance oracle from ADR-0049 SS1.3/1.4: the 29 rows
# where mutsu diverged from raku on `main` (227e38e4f, 2026-08-20) PLUS the 13
# invariants mutsu already got right (dual-oracled against `raku` again while
# writing this file). Slices 1-2 (this PR) fix the construction-site decay; a
# `# TODO: ADR-0049 ...` comment marks each row still expected to diverge, so
# later slices have a live regression net instead of a silently-omitted case.
#
# The invariant half is the important other side: it is what stops a later
# slice from "fixing" the divergence by purging `Nil` from Lists or from hole
# materialization -- `Nil` legitimately survives in a `List`/`Seq`/slurpy, and
# `.List` on an array hole is *supposed* to read back as `Nil`.

plan 45;

# === SS1.3 divergent rows (29) ===

# 01: a Nil literal element decays to Any at its own construction.
my @row01 = [Nil];
ok @row01 eqv [Nil], 'row 01: my @b = [Nil]; @b eqv [Nil]';

# 02: [Nil] eqv [Any] directly.
ok [Nil] eqv [Any], 'row 02: [Nil] eqv [Any]';

# 03: reading the decayed element back gives the Any type object.
is [Nil][0].WHAT, Any, 'row 03: [Nil][0].WHAT';

# 04: binding an anonymous literal preserves the decay.
my @row04 := [Nil];
is @row04[0].WHAT, Any, 'row 04: my @a := [Nil]; @a[0].WHAT';

# 05: passing an anonymous literal straight to a sub keeps the decay.
sub row05(@x) { @x[0].WHAT }
is row05([Nil]), Any, 'row 05: sub f(@x){@x[0].WHAT}; f([Nil])';

# 06: a scalar-bound anonymous literal is unaffected by the itemization.
my $row06 = [Nil];
is $row06[0].WHAT, Any, 'row 06: my $c = [Nil]; $c[0].WHAT';

# 07: a deliberately-stored Nil element (decayed to Any) still :exists.
ok ([Nil,1][0]:exists), 'row 07: [Nil,1][0]:exists';

# 08: :v on the decayed element yields the Any type object.
is ([Nil,1][0]:v), Any, 'row 08: [Nil,1][0]:v';

# 09: .head on a real array reads the decayed store, not a raw Nil.
is [Nil,1].head.WHAT, Any, 'row 09: [Nil,1].head.WHAT';

# 10: .map sees the decayed element too -- no reader-side compensation needed.
is [Nil,1].map({.WHAT}).head, Any, 'row 10: [Nil,1].map({.WHAT}).head';

# 11/12/13: .sort/.reverse/.flat all read the already-decayed store.
is [Nil,1].sort.raku, '(Any, 1).Seq', 'row 11: [Nil,1].sort.raku';
is [Nil,1].reverse.raku, '(1, Any).Seq', 'row 12: [Nil,1].reverse.raku';
is [Nil,1].flat.raku, '(Any, 1).Seq', 'row 13: [Nil,1].flat.raku';

# 14: .clone copies the already-decayed element.
is [Nil,1].clone[0].WHAT, Any, 'row 14: [Nil,1].clone[0].WHAT';

# 15: nested literal construction decays inside-out (ADR-0049 SS1.5).
ok [[Nil]] eqv [[Any]], 'row 15: [[Nil]] eqv [[Any]]';

# 16/17: Array.new(Nil) decays at its own untyped construction.
is Array.new(Nil)[0].WHAT, Any, 'row 16: Array.new(Nil)[0].WHAT';
ok (Array.new(Nil)[0]:exists), 'row 17: Array.new(Nil)[0]:exists';

# 18: .List materializing a *real* (decayed) element is now correct --
# previously this was "right answer for a hole, wrong answer for a value"
# (ADR-0049 SS1.3 row 18 commentary).
is [Nil].List.raku, '(Any,)', 'row 18: [Nil].List.raku';

# 19: THE data-loss bug (slice 1) -- a trailing-comma literal no longer drops
# its Nil element.
is [Nil,].elems, 1, 'row 19: [Nil,].elems (was silently dropped to 0)';

# 20/21: hash-literal and list-assign construction both decay the pair value.
ok {a=>Nil} eqv {a=>Any}, 'row 20: {a=>Nil} eqv {a=>Any}';
my %row21 = a=>Nil;
ok %row21 eqv {a=>Any}, 'row 21: my %g = a=>Nil; %g eqv {a=>Any}';

# 22/23: .values / .pairs read the decayed store, not a raw stored Nil.
is {a=>Nil}.values.head.WHAT, Any, 'row 22: {a=>Nil}.values.head.WHAT';
is {a=>Nil}.pairs.head.value.WHAT, Any, 'row 23: {a=>Nil}.pairs.head.value.WHAT';

# 24: a nested array-valued hash entry decays too (inside-out, same as row 15).
ok {a=>[Nil]} eqv {a=>[Any]}, 'row 24: {a=>[Nil]} eqv {a=>[Any]}';

# 25: .AT-KEY on a genuinely-*missing* key now goes through the same
# container-default compensation every other hash-key reader already had
# (ADR-0049 slice 5 -- AT-KEY previously had none at all).
my %row25;
is %row25.AT-KEY("missing").WHAT, Any, 'row 25: my %h; %h.AT-KEY("missing").WHAT';

# 26: AT-KEY on a key that holds a *decayed* (not missing) value already works
# from slices 1-2 -- the map genuinely contains Any now, so no missing-key
# compensation is even needed.
my %row26 = a=>Nil;
is %row26.AT-KEY("a").WHAT, Any, 'row 26: my %n = a=>Nil; %n.AT-KEY("a").WHAT';

# 27: an untyped `[Nil]` literal now decays to `[Any]` at its own
# construction (slice 2), so assigning it to a typed `Int @a` hits the
# ordinary element type check and dies -- exactly like raku. (ADR-0049 SS4
# slice 3 called this "the single most visible behaviour change"; it in fact
# already falls out of slices 1-2, because the literal no longer hands the
# assignment site a raw `Nil` to special-case leniently.)
{
    my $died = False;
    my $message = '';
    try {
        my Int @row27 = [Nil];
        CATCH { default { $died = True; $message = .message } }
    }
    ok $died && $message.contains('Int') && $message.contains('Any'),
        'row 27: my Int @a = [Nil] dies with an Int/Any type-check message';
}

# 28: `my %h{Int} = 1 => Nil` no longer dies -- the `Nil` *value* decays to
# the hash's `Any` value-type default before any type check sees it (the
# `Int` constraint here is on the *key*, which is unaffected).
{
    my $died = False;
    try {
        my %row28{Int} = 1 => Nil;
        CATCH { default { $died = True } }
    }
    ok !$died, 'row 28: my %h{Int} = 1 => Nil does not die';
}

# 29: TODO -- decay is per-container (the *inner* `[Nil]` literal decays to
# its own `Any`, not the *outer* variable's `is default(42)`), but the
# existing read-side `resolve_array_entry` chokepoint
# (src/vm/vm_var_ops.rs) unconditionally substitutes a non-Nil container
# default for ANY in-range `Package("Any")` element -- including one that is
# a genuinely-stored value, not a hole. ADR-0049 SS5.2 flags this exact
# rewrite as "a *different* bug in the same family ... deserve[ing] its own
# probe rather than being swept in" to this ADR, so it stays open past
# slices 1-2.
{
    todo 'row 29: per-container decay vs is-default read-side rewrite (ADR-0049 SS5.2 follow-up)';
    my @row29 is default(42) = [Nil];
    is @row29[0], Any, 'row 29: my @d is default(42) = [Nil]; @d[0]';
}

# === SS1.4 invariants (13 rows, 16 assertions -- I13 bundles 4 checks) ===
# These must stay green forever: they are the regression net that stops a
# later slice from purging Nil out of Lists or out of hole materialization.

# I1: a List element holding a literal Nil is unaffected by the store-decay
# rule (only real Array/Hash elements are containers). `.WHAT` on `Nil`
# itself is `Nil`, not a type object -- verified identical on raku and mutsu.
ok (1,Nil,2)[1].WHAT =:= Nil, 'I1: (1,Nil,2)[1].WHAT -- a List element stays Nil';

# I2: eqv on two Lists holding literal Nil.
ok (1,Nil,2) eqv (1,Nil,2), 'I2: (1,Nil,2) eqv (1,Nil,2)';

# I3: a slurpy `*@x` is List-backed, so it keeps a literal Nil too.
sub inv03(*@x) { @x[0].WHAT }
ok inv03(Nil,1) =:= Nil, 'I3: sub s(*@x){@x[0].WHAT}; s(Nil,1)';

# I4: an autovivification-gap array materializes its holes as Nil via .List.
my @inv04; @inv04[2] = 5;
is @inv04.List.raku, '(Nil, Nil, 5)', 'I4: my @a; @a[2]=5; @a.List.raku';

# I5: a direct element read on the same gap still vivifies to Any.
is @inv04[0].WHAT, Any, 'I5: my @a; @a[2]=5; @a[0].WHAT';

# I6: the same gap does not :exist.
nok (@inv04[0]:exists), 'I6: my @a; @a[2]=5; @a[0]:exists is False';

# I7: an untyped `my @n = 1, Nil, 3` list-assign already decays -- and,
# crucially, answers :exists the SAME way row 07's `[Nil, 1]` literal does
# (ADR-0049's sharpest statement of the pre-fix defect).
my @inv07 = 1, Nil, 3;
ok (@inv07[1]:exists), 'I7: my @n = 1, Nil, 3; @n[1]:exists';

# I8: :v on that same decayed element is Any.
is (@inv07[1]:v), Any, 'I8: my @n = 1, Nil, 3; @n[1]:v';

# I9: an explicitly `:delete`d slot does not :exist (a real hole, not a
# decayed value -- must stay distinguishable from row 07/I7 above).
my @inv09 = 1, 2, 3; @inv09[1]:delete;
nok (@inv09[1]:exists), 'I9: my @d=1,2,3; @d[1]:delete; @d[1]:exists is False';

# I10: assigning a literal Nil to an existing untyped element still decays to
# the array's own Any default.
my @inv10 = 1, 2, 3; @inv10[1] = Nil;
ok @inv10 eqv [1, Any, 3], 'I10: my @z=1,2,3; @z[1]=Nil; @z eqv [1,Any,3]';

# I11: assigning Nil to a typed-array element decays to the declared type
# object, not Any.
my Int @inv11 = 1, 2; @inv11[0] = Nil;
is @inv11.raku, 'Array[Int].new(Int, 2)', 'I11: my Int @t=1,2; @t[0]=Nil; @t.raku';

# I12: a typed-array *literal* list-assign decays its Nil elements the same
# way.
my Int @inv12 = 1, Nil, 3;
is @inv12.raku, 'Array[Int].new(1, Int, 3)', 'I12: my Int @a=1,Nil,3; @a.raku';

# I13: elems/gist/.Slip on a decayed one-Nil / two-element literal.
is [Nil].elems, 1, 'I13a: [Nil].elems';
is [1,Nil].elems, 2, 'I13b: [1,Nil].elems';
is [Nil].gist, '[(Any)]', 'I13c: [Nil].gist';
is [Nil].Slip.raku, 'slip(Any,)', 'I13d: [Nil].Slip.raku';
