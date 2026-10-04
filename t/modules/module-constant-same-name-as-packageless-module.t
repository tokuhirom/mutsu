use lib 't/lib';
use Test;

# Template::HAML: `Renderer` is a package-less module with file-scope
# `constant TRIM-BEFORE`; `DirectCodegen` is a `unit module` declaring its own
# `TRIM-BEFORE`. Once the first was loaded through a nested `use`, the second
# module's own constant was hidden from its routines ("Undeclared name").
plan 1;

use NestedUsesPackageless;
use UnitModuleTrimConsts;

is Wrapper.new.wrap("x"), "<x>", "unit module's own constants win over a same-named package-less module's";
