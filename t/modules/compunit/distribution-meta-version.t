use v6;
use Test;

# A META6.json-backed `$?DISTRIBUTION` fills in the identity keys the way
# Rakudo's CompUnit::Repository::Distribution does: `ver //= version`,
# `auth //= authority // author`, `api //= ''`. HTTP::Tiny builds its user
# agent from `$?DISTRIBUTION.meta<ver>` while its META6.json only has
# `version`, which warned and dropped the version.

use lib 't/fixtures/dist-meta-version';
use DistMetaVersion;

plan 6;

my %meta = dist-meta();
is %meta<ver>, '0.2.6', '<ver> falls back to <version>';
is %meta<version>, '0.2.6', '<version> is kept as written';
is %meta<auth>, 'mutsu', '<auth> falls back to <author>';
is %meta<api>, '', '<api> defaults to the empty string';

my $warned = False;
{
    CONTROL { when CX::Warn { $warned = True; .resume } }
    is Agent.new.agent, 'DistMetaVersion::Agent/0.2.6 Raku',
        'attribute default reads <ver> from a version-only META6.json';
}
nok $warned, 'building the agent string does not warn';
