use v6;
use Test;

# $?DISTRIBUTION must resolve to the owning module's distribution inside a routine
# body (including a role method composed into a class and compiled on demand after
# the module finished loading), not Nil. Regression for zef's `Zef::Pluggable`
# `!try-load`, which reads `$?DISTRIBUTION.meta<provides>{...}:exists`.

use lib 't/fixtures/dist-role/lib';
use DistRoleFixture;

plan 6;

my $c = make-consumer();

ok $c.dist-defined, 'role method: $?DISTRIBUTION is defined (not Nil)';
ok $c.provides-has('DistRoleFixture'),
    'role method: $?DISTRIBUTION.meta<provides> knows the provided module';
nok $c.provides-has('No::Such::Module'),
    'role method: $?DISTRIBUTION.meta<provides> reports a missing module as absent';

ok sub-dist-defined(), 'exported sub: $?DISTRIBUTION is defined';

# Calling the same role method again uses the cached compiled body — still defined.
ok $c.dist-defined, 'role method: still defined on second call (cached body)';

# A freshly constructed consumer resolves it too.
ok make-consumer().dist-defined, 'role method: defined on a second instance';
