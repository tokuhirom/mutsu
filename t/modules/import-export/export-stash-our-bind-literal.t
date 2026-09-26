use Test;

# An operator bound into a module's EXPORT stash under a literal key --
# `OUR::{'&infix:<%%%>'} := ...` / `OUR::«'&infix:<@~~>'» := ...`, in either
# the `package EXPORT::DEFAULT` or the nested `package EXPORT { package
# DEFAULT { } }` spelling -- is exported like an `our sub` declared there.
# The importer's parser must learn the operator: `1 @~~ 2` used to misparse
# silently (Data::Record's `&infix:<@~~>`, #9499).

plan 4;

use lib 't/lib';
use StashBindLiteralOpMod;

is (1 @~~ 2), '1~2', 'infix bound in the nested EXPORT { DEFAULT { } } stash';
is (3 %%% 4), 12, 'infix bound in EXPORT::DEFAULT with OUR::{...}';
is (¬¬ 0), True, 'a prefix operator bound the same way';
is &infix:<@~~>(5, 6), '5~6', 'the routine is also reachable by name';
