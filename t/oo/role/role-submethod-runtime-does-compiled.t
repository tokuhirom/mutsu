use Test;

plan 9;

# `$value does Role` / `$value but Role`, applied to a plain (non-Instance)
# value like an Int or Str, runs the role's BUILD/TWEAK submethods through
# `run_role_submethod` (src/runtime/types/roles.rs), which now executes the
# submethod's precompiled bytecode chunk instead of re-walking the raw AST on
# every composition (ADR-0019 D8-3). Every expectation below was checked
# against Rakudo v2026.06.

role BuildsScalar { has $.x; submethod BUILD { $!x = 10 } }
my $v = 5;
$v does BuildsScalar;
is $v.x, 10, 'BUILD assigns a scalar attribute on a does-mixed plain value';

role TweaksScalar { has $.y = 1; submethod TWEAK { $!y = $!y + 100 } }
my $s = "hi";
my $s2 = $s but TweaksScalar;
is $s2.y, 101, 'TWEAK sees the initialized value on a but-mixed plain value';
is $s, 'hi', 'the non-mutating `but` form leaves the original value untouched';

my @order;
role RecordsOrder {
    submethod BUILD { @order.push('B') }
    submethod TWEAK { @order.push('T') }
}
my $u = 1;
$u does RecordsOrder;
is-deeply @order.List, <B T>, 'BUILD runs before TWEAK, each exactly once';

my $outer = 0;
role WritesOuter { submethod BUILD { $outer = 42 } }
my $z = 1;
$z does WritesOuter;
is $outer, 42, 'a captured outer lexical write inside BUILD propagates out';

role BeatsDefault { has $.x = 5; submethod BUILD { $!x = 9 } }
my $b = 1;
$b does BeatsDefault;
is $b.x, 9, 'BUILD wins over an attribute initializer on a does-mixed value';

# A parameterized role's own type/value parameter must be visible to its
# BUILD/TWEAK submethod on this composition path -- previously only bound via
# class_role_param_bindings, which is keyed by class name and so never
# reachable from a plain (non-Instance) does/but target.
# (todo/tickets/role-submethod-runtime-does-parameterized-value.md)
role ParamBuild[$v] { has $.p; submethod BUILD { $!p = $v } }
my $q = 1;
$q does ParamBuild[42];
is $q.p, 42, "a parameterized role's BUILD sees its own type/value parameter";

role ParamTweak[$v] { has $.p; submethod TWEAK { $!p = $v * 2 } }
my $t = 1;
$t does ParamTweak[5];
is $t.p, 10, "a parameterized role's TWEAK sees its own type/value parameter";

role ParamTwo[$a, $b] { has $.x; has $.y; submethod TWEAK { $!x = $a; $!y = $b } }
my $two = 1;
$two does ParamTwo[10, 20];
is "{$two.x},{$two.y}", "10,20", 'a role with two type/value parameters binds both';
