use Test;

plan 6;

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
