use Test;

# An operator bound into a module's EXPORT stash under a key the module
# computes at run time -- `OUR::{'&postfix:<' ~ $code ~ '>'} := ...` in a
# loop (Moneys) -- exists only once the module body has run, so no static
# scan can list it. The importer's parse runs the module to learn the names,
# as rakudo does by compiling `use` at BEGIN time (#9500).

plan 5;

use lib 't/lib';
use StashBindDynamicOpMod :ALL;

is 5USD, '5 USD', 'a postfix operator with a computed name';
is 6EUR, '6 EUR', 'every name the loop computed is known';
is (1.5USD).chars, 7, 'the postfix binds inside a parenthesized term';
is (1 @@ 2), '1@@2', 'an infix operator with an interpolated name';
is &postfix:<EUR>(7), '7 EUR', 'the routine is also reachable by name';
