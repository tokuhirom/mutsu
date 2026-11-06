use Test;
use lib 't/fixtures/indirect-lookup-from-module/lib';
use IndirectLookupProbe;

# Found via UML::Translators: `to-plant-uml-spec('MyPackageClass')` does
# `::($packageName)` from the module's own routine for a `module` the PROGRAM
# declared. A top-level package/module of the program is a GLOBAL member, so an
# indirect lookup from another compunit finds it (a `my module` stays private).

plan 8;

module MM { class C { } }
package PP { }
class KK { }
my module LM { }

is look('MM'), 'MM', 'module declared by the program';
is look('PP'), 'PP', 'package declared by the program';
is look('KK'), 'KK', 'class declared by the program';
is look('MM::C'), 'MM::C', 'a class inside the program module';
is look-required('MM'), 'MM', 'require of an already declared module is harmless';
is look('LM'), 'Failure', 'a my-scoped module is not visible from another compunit';
is look('Nonexistent::Pkg'), 'Failure', 'an unknown name still fails';
is ::('MM').^name, 'MM', 'and from the program itself';
