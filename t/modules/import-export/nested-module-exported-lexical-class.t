use Test;

# A `my class ... is export` inside a non-`unit` `module` block, or inside a
# routine body, of a module file is exported like a top-level one.

use lib 't/lib';
use NestedModuleExportedClass;

plan 2;

is NestedModExp.hi, 'hi from module', 'class in a nested module block is exported';
is RoutineExp.hi, 'hi from sub', 'class in a routine body is exported';
