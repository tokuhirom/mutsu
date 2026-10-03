use Test;
use lib $*PROGRAM.parent(3).add('lib');
use UseTagColonpair::Defs;

plan 3;

# `module Inner is export { ... }` inside a module publishes it to importers.
is Inner::f(), 'inner', 'the exported inline module is visible by its short name';
is Inner.^name, 'UseTagColonpair::Defs::Inner', 'it is the nested package';

module Local is export { our sub g { 'local' } }
is Local::g(), 'local', 'is export on a mainline module parses';
