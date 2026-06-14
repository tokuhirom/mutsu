use v6;
use Test;

# VM-native default construction now covers `has $.x does Role` attributes
# (Track A ③ constructor). The native builder mixes each declared role into the
# attribute's value via the shared `apply_attribute_does_role_mixins` helper —
# the same approximation the full constructor uses (mutsu mixes into the value,
# not the Scalar container; enough for method dispatch and `~~ Role`). The
# native and interpreter paths share the helper, so they stay byte-identical.
#
# NOTE: this pins mutsu's *documented approximation*, NOT raku conformance.
# Raku mixes the role into the (immutable) Scalar container, so providing or
# defaulting a value on a `does Role` attribute raises "Cannot assign to an
# immutable value" in raku. mutsu has no per-attribute container and carries the
# mixin on the value instead, so it accepts the value. This test asserts that
# mutsu's native constructor reproduces mutsu's interpreter behavior exactly.

plan 11;

role R1 { method m1 { "m1" } }
role R2 { method m2 { "m2" } }

# --- a single role mixed into an attribute value ---
class A { has $.x does R1; }
my $a = A.new(x => 42);
is $a.x, 42, 'does-Role attribute keeps its provided value';
is $a.x.m1, "m1", 'a role method dispatches into the attribute value';
ok $a.x ~~ R1, 'the attribute value does the role';

# --- multiple roles on one attribute ---
class B { has $.y does R1 does R2; }
my $b = B.new(y => "v");
is $b.y.m1 ~ $b.y.m2, "m1m2", 'both roles are mixed into the attribute';
ok $b.y ~~ R1 && $b.y ~~ R2, 'the value does both roles';

# --- does-Role with a default value ---
class C { has $.z does R1 = 100; }
is C.new.z, 100, 'does-Role attribute uses its default';
is C.new.z.m1, "m1", 'the default value still does the role';

# --- does-Role alongside plain attributes ---
class D { has $.a; has $.b does R1; }
my $d = D.new(a => 1, b => 2);
is $d.a, 1, 'a plain attribute is unaffected';
is $d.b, 2, 'the does-Role attribute keeps its value';
is $d.b.m1, "m1", 'only the does-Role attribute carries the mixin';
nok $d.a ~~ R1, 'the plain attribute does not do the role';
