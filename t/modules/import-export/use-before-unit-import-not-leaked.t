use Test;
use lib 't/lib';

# A `use` is lexical to the compunit that says it (#9587): a module whose
# `use` comes before its `unit class`, or which has no `unit` declarator at
# all, must not leak the imported routines into its importer.

use ImportLeakPreUnit;
use ImportLeakShadow;
# A private `sub leak-ex` declared before ImportLeakBare imports the name.
use ImportLeakPrivate;
use ImportLeakBare;

plan 7;

is ImportLeakPreUnit.e, 'A', 'a pre-unit import is visible to the module itself';
is leak-shadow(), 'H',
    'a later module may declare its own sub with the name another module imported';
is leak-private-call(), 'T', 'a private same-named sub stays private to its module';
is ImportLeakBare.e, 'A', 'a bare-file module method sees its own import';
is bare-leak-call(), 'A', 'a bare-file module exported sub sees its own import';
dies-ok { EVAL 'leak-ex(1)' }, 'the imported routine does not leak to the importer';
nok MY::<&leak-ex>:exists, 'no &leak-ex binding in the importer';
