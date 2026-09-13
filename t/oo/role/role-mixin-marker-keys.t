use v6;
use Test;

# The mixin registry is a second family of `__mutsu_*` magic keys, and it is a
# different shape from the per-binding env keys pinned by
# `t/vm/binding/var-metadata-key-namespaces.t`: these live in the `MixinOverrides`
# map hanging off a mixed-in value, they are `String`-keyed rather than
# `Symbol`-keyed, and they join their prefix to the role or attribute name with
# `__` rather than `::` -- `__mutsu_role__Stringy`, `__mutsu_attr__$!x`.
#
# Issue #8087 stage 3 moved all 78 of their hand-built `format!` sites onto
# `MetaNs` (src/runtime/meta_ns.rs). Nothing about that move is visible to the
# type checker: the writer inserts under one spelling and the reader probes
# another, and a mismatch does not error, it just means the marker is never
# found again -- `does` goes quiet, an attribute stops resolving, two roles
# stop ordering against each other.
#
# So each case below is one namespace's observable behaviour, chosen so that it
# fails if the marker is written but not found. The exact spellings are pinned
# separately by meta_ns.rs's unit tests; this file pins what they drive.
#
# The `__` separator in particular is load-bearing and nearly collides:
# `__mutsu_role__` is one character from being a prefix of `__mutsu_role_seq__`,
# and `__mutsu_attr__` from `__mutsu_attr_trait__`. `MixinOverrides` really does
# enumerate the map with `strip_prefix`, so a namespace that swallowed another's
# entries would corrupt every case here at once.

plan 14;

# __mutsu_role__ -- the "this value does this role" marker. The single
# most-probed key in the registry: every `does`, every method dispatch onto a
# mixin and every type smartmatch reads it.
{
    role Greet { method greet { 'hi' } }
    my $x = 42 but Greet;
    ok $x ~~ Greet, 'a runtime mixin is found by the role marker';
    is $x.greet, 'hi', 'and its method dispatches';
    ok !(42 ~~ Greet), 'an unmixed value does not carry the marker';
}

# __mutsu_role_seq__ -- a monotonic application-order stamp. Rakudo resolves a
# method-name collision between two mixed-in roles by later-wins precedence, so
# this is what decides the answer; a lost stamp would make the order arbitrary.
{
    role A { method who { 'A' } }
    role B { method who { 'B' } }
    my $ab = 1 but A;
    $ab = $ab but B;
    is $ab.who, 'B', 'the later-applied role wins a method collision';
    my $ba = 1 but B;
    $ba = $ba but A;
    is $ba.who, 'A', 'and the order really is application order, not name order';
}

# __mutsu_role_typeargs__ / __mutsu_role_param__ -- a parameterised role records
# the type arguments it was applied with, and each bound parameter separately.
{
    role Holder[::T] {
        method accepts($v) { $v ~~ T }
    }
    my $ints = 0 but Holder[Int];
    ok $ints.accepts(7), 'a parameterised role sees its bound type parameter';
    ok !$ints.accepts('x'), 'and rejects a value of another type';
    ok $ints ~~ Holder[Int], 'the type arguments are matched against, not just the role';
}

# __mutsu_role_id__ -- which RoleDef this application resolved to. Two
# parameterisations of one role are different types and must not collide.
{
    role Boxed[::T] { method t { T.^name } }
    my $i = 0 but Boxed[Int];
    my $s = 0 but Boxed[Str];
    is $i.t, 'Int', 'one parameterisation keeps its own role id';
    is $s.t, 'Str', 'and the other keeps its own';
}

# __mutsu_attr__ -- a role attribute composed into the mixin. Read on every
# accessor call and written on every assignment through one.
{
    role Counter { has $.count is rw = 0 }
    my $c = 'x' but Counter;
    is $c.count, 0, 'a composed role attribute has its default';
    $c.count = 5;
    is $c.count, 5, 'and an assignment through the accessor is found again';
}

# __mutsu_role_group__ -- the candidate group a multiply-declared role belongs
# to, which is how one `does` sees all the arities as one role.
{
    role Multi { method tag { 'plain' } }
    role Multi[::T] { method tag { 'parametric' } }
    my $p = 1 but Multi[Int];
    is $p.tag, 'parametric', 'the parametric candidate is picked for an argument';
    my $b = 1 but Multi;
    is $b.tag, 'plain', 'and the bare candidate for none';
}
