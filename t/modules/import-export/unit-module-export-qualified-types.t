use lib 't/lib';
use Test;
use UnitExport::Cfg;

plan 3;

# `unit module A::B is export;` wraps the declaration in a header wrapper;
# the types declared after it must still be qualified into the unit package.
is UnitExport::Cfg::type-name(), 'UnitExport::Cfg::INI', 'grammar is qualified into the unit module';
is UnitExport::Cfg::parse('a'), 42, 'the grammar and its actions resolve inside the module';
ok UnitExport::Cfg::INI.^can('parse'), 'the qualified grammar is reachable by its full name';
