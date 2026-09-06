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

plan 9;

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

# `use lib`/`use` above already loaded the unit; a `require` of the same unit
# must not publish the private helper either.
require ::('PrivateSubMod');
is secret-helper(1), 101,
    'require does not publish the loaded unit private helper into this scope';

# An exported routine keeps working after the require, i.e. the seclusion did
# not take the export with it.
is visible-helper(5), 15,
    'the exported routine still works after a require of the same unit';

done-testing;
