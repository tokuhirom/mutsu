use v6;
use Test;

plan 4;

# `$*REPO.need($spec)` must accept a CompUnit::DependencySpecification that
# carries auth/api/version matchers (an object instance), not only the bare
# short-name form. zef's plugin loader builds the matcher-bearing form for every
# plugin, so a FileSystem repo that rejected it left zef unable to load any
# fetch/repository backend.

use lib $?FILE.IO.parent.add('lib-need-instance').Str;

my $spec = CompUnit::DependencySpecification.new(
    short-name      => 'NeedMe',
    auth-matcher    => True,
    api-matcher     => '*',
    version-matcher => '*',
);

isa-ok $spec, CompUnit::DependencySpecification,
    'a matcher-bearing depspec is a CompUnit::DependencySpecification';
is $spec.short-name, 'NeedMe', 'the depspec exposes its short-name';

my $cu = $*REPO.need($spec);
ok $cu.defined, '$*REPO.need resolves a matcher-bearing depspec from the FileSystem repo';

# The bare short-name form keeps working too.
my $cu2 = $*REPO.need(CompUnit::DependencySpecification.new(short-name => 'NeedMe'));
ok $cu2.defined, '$*REPO.need still resolves the bare short-name form';
