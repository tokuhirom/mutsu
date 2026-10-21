use Test;
use lib $?FILE.IO.parent(2).add('lib').Str;

# A loaded module's routine is declared at the top level of its own compunit,
# so its lexical outer is that file's scope, not the program that called it
# (ADR-12529 phase 3). Its `EVAL` and `::()` lookups read neither the
# program's own top-level lexicals nor the names the program's top-level
# `use`s imported; they still see the module's own types and package.
# Every expectation was checked against rakudo.

use ModuleRoutineScopeExporter;
use ModuleRoutineScopeProbe;

my $importer-var = 'importer';

plan 10;

is probe-eval-var(), 'not visible', "a module routine's EVAL does not read the program's lexical";
is probe-indirect-var(), 'not visible', "nor does its ::('\$x')";
is probe-eval-type(), 'not visible', "its EVAL does not see a type the program imported";
is probe-indirect-type(), 'not visible', "nor does its ::('Type')";
is probe-eval-constant(), 'not visible', "nor a constant the program imported";
is probe-own-class-eval(), 'own', "its EVAL sees the module's own class by its short name";
is probe-own-class-indirect(), 'own', "so does its ::('OwnClass')";
is probe-own-package(), 'ModuleRoutineScopeProbe', "and the module's own package name";
is probe-own-typed-eval(), 7, "a class its EVAL declares constrains a variable there";
is ImportedType.hi, 'imported', 'the program itself still sees its import';
