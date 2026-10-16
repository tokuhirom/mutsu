use Test;

# ADR-11276 §9.117: the distributions' methods are rows of the method table,
# found by their owner. A module's `$?DISTRIBUTION` is a
# `CompUnit::Repository::Distribution`, as in Rakudo 2026.09.

use lib 't/fixtures/dist-meta-version';
use DistMetaVersion;

plan 12;

my $d = dist();
is $d.^name, 'CompUnit::Repository::Distribution', '$?DISTRIBUTION is a CompUnit::Repository::Distribution';
ok $d ~~ Distribution, '... which does Distribution';
is $d.meta<name>, 'DistMetaVersion', '.meta';
is $d.Str, 'DistMetaVersion:ver<0.2.6>:auth<mutsu>:api<>', '.Str is the identity';
is $d.dist-id, $d.Str, '.dist-id is the identity';
isa-ok $d.prefix, IO::Path, '.prefix is an IO::Path';
ok $d.content('lib/DistMetaVersion.rakumod').IO.e, '.content finds a file of the distribution';
ok $d.^can('meta'), '.^can sees CompUnit::Repository::Distribution.meta';

my $h = Distribution::Hash.new({ name => 'A', ver => '1' }, prefix => 't'.IO);
is $h.meta<name>, 'A', 'Distribution::Hash.meta';
isa-ok $h.prefix, IO::Path, 'Distribution::Hash.prefix';
like $h.content('README').IO.Str, /'README'/, 'Distribution::Hash.content';
ok $h.^can('content'), '.^can sees Distribution::Hash.content';
