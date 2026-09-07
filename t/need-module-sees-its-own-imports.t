use v6;
use Test;
use lib $*PROGRAM.parent.add('lib');
use MONKEY-SEE-NO-EVAL;

need NeedOwnImports::Consumer;
use NeedOwnImports::UsedConsumer;

plan 5;

# A module's routines must be able to resolve the routines THAT MODULE
# imported, however the module was loaded and whatever the loader's own scope
# later does.
#
# 1. `need Foo` used to set `suppress_exports` for the whole load, which also
#    silenced the `is export` declarations of every module Foo itself `use`s --
#    so a routine declared in Foo saw nothing Foo had imported. `need
#    DBIish::CommonTesting` (whose file opens with `use Test;`) died with
#    "Unknown function: diag" inside its own method.
# 2. A scope restore (a block, an `EVAL "use ..."` -- which is what Test's
#    `use-ok` compiles to) rolled the imported aliases back out of the routine
#    registry while `loaded_modules` still claimed the module was loaded, so
#    the program's own later `use` was a no-op that could not put them back.

is NeedOwnImports::Consumer.new.go, 'provided(42)',
    'a needed module\'s method sees the module\'s own imports';

ok ::('&go-sub') ~~ Failure,
    'need still imports nothing into the needer';

is NeedOwnImports::UsedConsumer.new.go, 'provided(1)',
    'a used module\'s method sees the module\'s own imports';

is used-go(), 'provided(2)',
    'and use does import the module\'s own export';

# The EVAL'd-`use` half: the first load happens inside a scope that is then
# torn down, and the second `use` is the no-op that used to leave the module
# half-loaded.
EVAL 'use NeedOwnImports::EvalConsumer';
{
    use NeedOwnImports::EvalConsumer;
    is NeedOwnImports::EvalConsumer.new.go, 'provided(9)',
        'a module first loaded inside an EVAL keeps its own imports';
}
