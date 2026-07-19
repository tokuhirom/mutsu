use Test;

# Pin for the (B) per-store env-write gate method-lvalue self-writeback fix
# (docs/lexical-scope-slot-campaign.md, "(B) per-store env-write gate — burndown",
# method-lvalue cluster).
#
# After an `__mutsu_assign_method_lvalue` call (`$x.attr = v`), the call site pulls
# `env[target]` back into the caller's local slot to keep it coherent — assuming the
# lvalue builtin wrote `env[target]`. Some do NOT: `Failure.handled = True` only
# flips a global registry keyed by the instance id, leaving `env[target]` untouched.
# Under the MUTSU_GATE_LOCAL_ENV_WRITE gate `my $f = Failure.new` leaves env at its
# `Any` decl seed, so the unconditional pull clobbered the live instance slot with
# `Any` (`$f` became Any). The fix only applies the pull when the builtin genuinely
# changed `env[target]` during the call (an env_changed guard). Passes gate-OFF
# (default) and would fail gate-ON before the fix.

plan 6;

# Failure.handled = True does not write env[target]; the slot must survive.
my $f = Failure.new("boom");
$f.handled = True;
is $f.handled, True, 'Failure.handled can be set';
$f.so;
isa-ok $f, Failure, 'the Failure instance slot survives the lvalue assign';

# A normal lvalue writeback that DOES change env[target] still applies.
my @a = 1, 2, 3;
@a.head = 99;
is @a, [99, 2, 3], 'array head lvalue writeback still lands';

my %h = x => 1, y => 2;
%h<x> = 5;
is %h<x>, 5, 'hash element assign still lands';

# Interleaving a Test-fn call between the lvalue assign and a later read.
my $f2 = Failure.new("again");
$f2.handled = True;
ok $f2.handled, 'handled state persists across a following assertion';
$f2.so;
is $f2.exception.message, "again", 'the exception is still reachable on the slot';
