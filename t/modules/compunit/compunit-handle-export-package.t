use lib 't/lib';
use Test;

# `CompUnit::Handle.export-package` answers the unit's EXPORT stash (#11002).

plan 6;

my $comp = $*REPO.need(CompUnit::DependencySpecification.new(:short-name<ExportPackageMod>));
my $pkg = $comp.handle.export-package;

isa-ok $pkg, Stash, 'export-package is a Stash';
is $pkg.keys.sort.join(','), 'ALL,DEFAULT,tag', '... of the export tags';
is $pkg<DEFAULT>.WHO.keys.sort.join(','), '&epm-foo', 'DEFAULT holds the default exports';
is $pkg<tag>.WHO.keys.sort.join(','), '&epm-bar', 'a tag holds its own exports';
is $pkg<DEFAULT>.WHO<&epm-foo>(), 42, 'an exported routine is callable from the stash';
throws-like { EVAL 'epm-foo()' }, X::Undeclared::Symbols,
    'the repository need imports nothing into the caller';
