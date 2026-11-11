use lib 't/lib';
use Test;

# A BEGIN phaser of a module installs the module's `&EXPORT` with
# `$*CU.merge-generated-lexical-declaration` and finds its own lexical
# `EXPORTHOW` with `$*R.resolve-lexical-constant` (#15371, CompUnit::Util's
# `steal-export-sub` and `re-exporthow`).

plan 3;

use GeneratedExportHook;

is generated(), 'from the generated EXPORT', 'the generated EXPORT hook ran for the importer';
is found-exporthow(), 'Perl6::Metamodel::ClassHOW',
    "the resolver answered the unit's own EXPORTHOW";
my gadget Widget { }
ok Widget.HOW ~~ Metamodel::ClassHOW, "the unit's own declarator is exported";
