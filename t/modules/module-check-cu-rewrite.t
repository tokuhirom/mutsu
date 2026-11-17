use Test;
use lib 't/lib';
use CheckRewriteModule;

# A module whose CHECK phase rewrites `$*CU` runs the rewritten tree, as the
# main unit does: rakudo compiles the unit from `$*CU` after its CHECK phase.
# Red's `CHECK red-compile` (t/124-compiler-callables) attaches compiled query
# templates this way (#17886).

plan 7;

is f(), 2, 'an exported routine runs its rewritten body';
is g(), 4, 'every rewritten call in it does';
is &f(), 2, 'so does the routine read through its &name';
is CheckRewriteModule::qualified(), 2, 'an our-scoped routine is rewritten too';
is $CheckRewriteModule::module-value, 2, "the module's mainline ran the rewritten tree";
is package-name(), 'CheckRewriteModule', 'the restarted module keeps its own package';
is check-package(), 'CheckRewriteModule',
    "a routine's CHECK-time compile-time-value is declared in the module's package";
