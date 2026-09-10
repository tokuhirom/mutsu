use v6;
use Test;
use lib 't/lib';
use PrivateSubMod;

# A loaded compilation unit's package-less top-level `sub` that is NOT
# `is export`ed is lexical to that unit. It must:
#   - stay reachable from the unit's OWN routines (an exported sub, a method of
#     a class the unit declares, a block inside one of those),
#   - be invisible to the scope that `use`d/`require`d the unit, and
#   - never displace a same-named routine the loading scope declares itself.
#
# Every expectation below was verified against Rakudo.
# See runtime/unit_private_routines.rs.

plan 14;

# The loading scope declares its own routine of the same name as the module's
# private helper. The two are independent lexicals.
sub secret-helper($n) { $n + 100 }

is visible-helper(2), 6,
    'the module exported sub reaches its own private helper';
is PrivateSubBox.new(n => 3).tripled, 9,
    'a method of a class the module declares reaches the private helper';
is via-block(), '3,6',
    'a block inside a module routine reaches the private helper';

is secret-helper(1), 101,
    "the loading scope's own same-named routine is not displaced";
is visible-helper(2), 6,
    "and the module still reaches its own after the caller's has been called";

# The ticket's own repro: a private helper the loading scope does not declare
# itself is simply not there. (`EVAL`, because a bare call to an undeclared
# routine is a compile-time error for the whole file.)
my $bare = try EVAL 'hidden-only(1)';
nok $bare.defined,
    'a private helper the loading scope never declared is not callable here';
is visible-hidden-only(2), 14,
    'and the module own exported wrapper still reaches it';

# The other side of the line: `our sub` in a package-less compunit really is a
# GLOBAL stash entry, so it stays reachable here without being exported.
is our-scoped-helper(2), 22,
    'an our-scoped top-level sub is NOT secluded';

# `use lib`/`use` above already loaded the unit; a `require` of the same unit
# must not publish the private helper either.
require ::('PrivateSubMod');
is secret-helper(1), 101,
    'require does not publish the loaded unit private helper into this scope';

# An exported routine keeps working after the require, i.e. the seclusion did
# not take the export with it.
is visible-helper(5), 15,
    'the exported routine still works after a require of the same unit';

# A block the module hands to a native callback taker runs long after the load,
# dispatched from the emitting scope (here, the main script). It is still
# lexically inside the module, so it must reach the module's private helper.
my $tap-src = Supplier.new;
my @tapped := tapped-values($tap-src.Supply);
$tap-src.emit(2);
is @tapped.join(','), '6',
    'a .tap callback declared in the module reaches the private helper';

my $sup-src = Supplier.new;
my @emitted;
tripling-supply($sup-src.Supply).tap(-> $v { @emitted.push($v) });
$sup-src.emit(3);
$sup-src.emit(4);
is @emitted.join(','), '9,12',
    'a supply/whenever body declared in the module reaches the private helper';

# The role is composed HERE, in the loading scope, which re-runs its body. Both
# the role's own lexical sub and a method of a class nested in its body must
# still resolve against the module's compunit, not this one.
my class Composed does PrivateSubRole { }
is Composed.new.role-helper(3), 15,
    "a composed role method reaches the role's own compunit-local sub";
is Composed.new.nested-helper(4), 12,
    'a method of a class nested in a composed role body reaches the private helper';

done-testing;
