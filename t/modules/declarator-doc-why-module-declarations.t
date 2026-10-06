use v6;
use lib 't/lib';
use Test;
use DeclDocWhyA;
use DeclDocWhyB;
use DeclDocWhyC;

# A module's declarator docs used to be dropped when its load ended, so `.WHY`
# on a declaration of the module was Nil, from the importer and from the
# module's own routines alike. The docs of every loaded module are kept under
# the module's compilation unit, so two modules documenting the same name
# stay apart and neither answers for the importer's own declaration.

plan 18;

# A routine, seen from the importer and from inside its module.
is &inc-a.WHY.Str, 'Adds one.', '.WHY on an imported routine';
is why-of-inc-a(), 'Adds one.', '.WHY on a routine, read from inside its own module';
ok &inc-a.WHY =:= &inc-a.WHY, 'every .WHY on one routine is the same object';

# Other declaration kinds.
is DeclDocWhyClass.WHY.Str, 'A documented class.', '.WHY on a module\'s class';
is why-of-class(), 'A documented class.', '... from inside the module';
ok DeclDocWhyClass.WHY =:= DeclDocWhyClass.WHY, 'the class\'s .WHY is one object';
is DeclDocWhyClass.^find_method('m').WHY.Str, 'A documented method.', '.WHY on a module\'s method';
is why-of-method(), 'A documented method.', '... from inside the module';
is DeclDocWhyClass.^find_method('mm').candidates[0].WHY.Str, 'First multi candidate.',
    'a multi candidate keeps its own doc';
is DeclDocWhyClass.^find_method('mm').candidates[1].WHY.Str, 'Second multi candidate.',
    '... and so does its sibling';
is DeclDocWhyRole.WHY.Str, 'A documented role.', '.WHY on a module\'s role';

# Same name in two modules, and in the importer.
is why-in-b(), q{B's doc.}, 'a module\'s routine reads its own doc ...';
is why-in-c(), q{C's doc. / B's doc.}, '... even when another module has a routine of the same name';

#| The importer's own doc.
sub shared-name($x) { 3 }
is &shared-name.WHY.Str, 'The importer\'s own doc.', 'the importer\'s routine of that name reads its own doc';
is why-in-b(), q{B's doc.}, '... and a module still reads its own after the importer declared one';

# An undocumented declaration has no doc, whichever side it is declared on.
sub undocumented-here($x) { 1 }
nok &undocumented-here.WHY.defined, 'an undocumented routine of the importer has no doc';
nok why-of-undocumented(), 'an undocumented routine of a module has no doc';

# The importer's own docs are untouched by the loads.
#| Local doc.
sub local-sub { 1 }
is &local-sub.WHY.Str, 'Local doc.', 'the importer\'s own doc comments still work';
