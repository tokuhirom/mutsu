use Test;

# #14173: CompUnit::Handle.export-how-package publishes the unit's EXPORTHOW
# stash (CompUnit::Util's `re-exporthow` calls it).
use lib 't/lib';

plan 5;

my $h = $*REPO.need(
    CompUnit::DependencySpecification.new(short-name => 'ExportHowStuff')
).handle;
my $how = $h.export-how-package;
isa-ok $how, Stash, 'a unit with EXPORTHOW answers a Stash';
is $how.keys.sort.join(','), 'DECLARE', 'the stash holds the directive';
is $how<DECLARE>.WHO.keys.sort.join(','), 'pokemon', 'the directive holds the declarator';
ok $how<DECLARE>.WHO<pokemon>.^name.ends-with('Metamodel::ClassHOW'), 'the declarator is its HOW';

my $plain = $*REPO.need(
    CompUnit::DependencySpecification.new(short-name => 'EnumHolder')
).handle;
nok $plain.export-how-package.defined, 'a unit without EXPORTHOW answers a type object';
