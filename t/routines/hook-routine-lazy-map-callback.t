use lib 't/lib-hook-routine-lazy';
use Test;

# A routine a `sub EXPORT` hook hands a module's top-level `use` belongs to
# that module's unit: a closure of the unit that runs after its routine
# returned (a lazy `map` callback) still calls it, in whatever frame forces
# it. The importing unit's own view stays unchanged: its caller never
# imported the name. Expected values are the `raku` output.

plan 3;

use HookRoutineLazyUser;

is-deeply via-hook([1, 2]).list, (101, 102), 'a lazy map callback calls the hook routine';
is-deeply via-hook([3]).list.list, (103,), 'the callback is reusable';
throws-like { EVAL 'hook-fn2(1)' }, X::Undeclared::Symbols, 'the importer of the module never got the name';
