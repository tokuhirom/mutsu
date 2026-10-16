use lib 't/lib';
use Test;

# ADR-11276 §9.117: `CompUnit`'s and `CompUnit::Handle`'s methods are rows of
# the method table, found by their owner. Answers checked against Rakudo 2026.09.

plan 9;

my $cu = $*REPO.need(CompUnit::DependencySpecification.new(:short-name<ExportPackageMod>));
isa-ok $cu, CompUnit, 'need answers a CompUnit';
is $cu.short-name, 'ExportPackageMod', '.short-name';
is $cu.Str, 'ExportPackageMod', '.Str is the short-name';
is $cu.from, 'Perl6', '.from of a FileSystem need';
isa-ok $cu.precompiled, Bool, '.precompiled';
isa-ok $cu.handle, CompUnit::Handle, '.handle';
ok $cu.^can('repo-id'), '.^can sees CompUnit.repo-id';
ok $cu.handle.^can('globalish-package'), '.^can sees CompUnit::Handle.globalish-package';
dies-ok { CompUnit.handle }, 'a CompUnit type object has no handle';
