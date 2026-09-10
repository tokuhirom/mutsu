use Test;

# Pin for the (B) per-store env-write gate class/role-method captured-outer fix
# (docs/lexical-scope-slot-campaign.md, "(B) per-store env-write gate — burndown").
#
# A class/role METHOD body is compiled lazily off its `RegisterClass`/`RegisterRole`
# op, so the defining frame's `compute_needs_env_sync` cannot see which enclosing
# lexicals the body reads by name. Such a body reads an outer lexical
# (`my $base = 100; class T { method calc($n) { $base + $n } }`) from the defining
# frame's env by name at call time. Under MUTSU_GATE_LOCAL_ENV_WRITE the outer
# `my $base = 100` store skips its env mirror, so the method read the decl seed
# (Any/0) instead of 100. The fix folds every local of a class/role-defining frame
# into `needs_env_sync` under the gate (mirroring the named-sub fold), keeping the
# store's env mirror current. Gate OFF (default) is byte-identical. This passes
# gate-OFF and would fail gate-ON before the fix.

plan 5;

# Class method reads a captured outer scalar.
my $base = 100;
class T { method calc(Int $n) { $base + $n } }
class D { has $.v; method run(Int $n) { $!v = T.calc($n); $!v } }
is D.new.run(5), 105, 'class method reads a captured outer lexical';

# Type-object method with a captured outer, result stored to an attribute.
my @registry;
class Status { has Int $.code; method CALL-ME(Status:U: Int $n) { @registry[$n] } }
@registry[7] = Status.new(code => 77);
class Response {
    has $.status;
    method set-status(Int $n) { $!status = Status($n); self }
}
is Response.new.set-status(7).status.code, 77,
    'attr assigned from a CALL-ME-on-type-object that reads a captured outer';

# Captured outer array read from inside a method.
my @vals = 10, 20, 30;
class Sum { method total { @vals.sum } }
is Sum.total, 60, 'class method reads a captured outer array';

# Role method reads a captured outer lexical.
my $mult = 3;
role Scale { method scale(Int $n) { $n * $mult } }
class Widget does Scale { }
is Widget.scale(4), 12, 'role method reads a captured outer lexical';

# Captured outer mutated between two method calls stays live.
my $offset = 1;
class Adder { method add(Int $n) { $n + $offset } }
my $a = Adder.add(10);
$offset = 5;
my $b = Adder.add(10);
is "$a,$b", "11,15", 'captured outer mutation is visible to a later method call';
