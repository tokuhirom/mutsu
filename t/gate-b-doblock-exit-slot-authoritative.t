use Test;

# Pin for the (B) per-store env-write gate block-exit clobber fix
# (docs/lexical-scope-slot-campaign.md, "(B) per-store env-write gate — burndown").
#
# A scope-isolating do-block / string-interpolation block (`"{ $obj.a }"`) exits
# through `exec_do_block_expr_op` (and a bare `{ ... }` through
# `exec_block_scope_op`). Both used to end the block by restoring the whole
# `locals` snapshot and then re-seeding EVERY slot from the restored `env` by
# name. Under the MUTSU_GATE_LOCAL_ENV_WRITE gate a `my $obj = Class.new(...)`
# skips its env mirror, so `env["obj"]` keeps the `my` decl seed (Any); the
# unconditional re-seed then clobbered the live instance slot with Any. In a
# single interpolation with two method calls (`"{ $obj.a },{ $obj.b }"`) the
# FIRST block's exit clobbered `$obj`, so the SECOND `$obj.b` died
# "No such method 'b' for invocant of type 'Any'".
#
# The fix makes the block exit slot-authoritative under the gate: a propagating
# outer var keeps its live in-block slot value; only the block's own isolated
# declarations and internal/special/dynamic slots revert. Gate OFF (the default)
# is byte-identical (whole-array restore + env re-seed). This passes gate-OFF and
# would fail gate-ON before the fix.

plan 8;

class Point {
    has $.x;
    has $.y;
    submethod TWEAK { $!x = $!x * 2 }
}

# Two method calls on one instance inside a single interpolation — the exact
# shape that regressed. Each `{ ... }` is a scope-isolating do-block; the first
# one's exit must not clobber `$p`.
my $p = Point.new(x => 3, y => 5);
is "{$p.x},{$p.y}", "6,5", 'two method calls in one interpolation see a live instance';
is "{$p.y}/{$p.x}/{$p.y}", "5/6/5", 'three method calls in one interpolation';

# A base-first parent/child TWEAK constructor, then a two-call interpolation.
class Parent { has $.a; submethod TWEAK { $!a *= 10 } }
class Child is Parent { has $.b; submethod TWEAK(:$b) { $!b = $b + 1 } }
my $c = Child.new(a => 4, b => 7);
is "{$c.a}:{$c.b}", "40:8", 'parent/child TWEAK instance survives a two-call interpolation';

# An outer-var mutation inside a bare block must still persist (not reverted).
my $acc = 1;
{ $acc = $acc + 100 }
is $acc, 101, 'outer var mutated inside a bare block persists';

# A block-local declaration must not leak past the block.
my $outer = 'keep';
{ my $outer = 'inner'; is $outer, 'inner', 'inner shadow visible inside block' }
is $outer, 'keep', 'block-local declaration does not leak out';

# Interpolation that both reads an instance and a plain scalar.
my $n = 41;
is "{$p.x}+{$n}", "6+41", 'interpolation mixes instance method call and plain scalar';

# Method call inside a `do { }` block returning to a later use of the same var.
my $q = Point.new(x => 10, y => 20);
my $sum = do { $q.x } + do { $q.y };
is $sum, 40, 'do-block method calls do not clobber the instance between blocks';
