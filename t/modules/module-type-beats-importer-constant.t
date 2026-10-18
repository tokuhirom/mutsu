use Test;

# #13628: a module body runs against its importer's scope in mutsu, so a
# constant the importer declares under the name of a type the module
# declares (`constant MtoicRegistry = ModuleTypeOverImporterConstant::
# MtoicRegistry`) must not answer the module's own bareword. The module means
# its own type, in its body and in its routines. Checked against rakudo.
plan 5;

use lib 't/lib';
use ModuleTypeOverImporterConstant;
constant MtoicRegistry = ModuleTypeOverImporterConstant::MtoicRegistry;

is $ModuleTypeOverImporterConstant::body-name,
    'ModuleTypeOverImporterConstant::MtoicRegistry',
    'the module body names its own type';
is mtoic-suites().elems, 2, 'an instance built in the module body is of its own type';
is mtoic-type-name(), 'ModuleTypeOverImporterConstant::MtoicRegistry',
    'a module routine names its own type';
is mtoic-shadowed(), 'own term', "a routine's own sigilless term still shadows the type";
is MtoicRegistry.^name, 'ModuleTypeOverImporterConstant::MtoicRegistry',
    "the importer's constant is the type";
