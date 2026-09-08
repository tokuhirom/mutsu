use v6;
use MONKEY-SEE-NO-EVAL;
use Test;

# A module whose FIRST load happens inside an `EVAL` used to lose the routines it
# and its dependencies imported: `eval_eval_string` rolls the routine registry
# back, `loaded_modules` is deliberately never rolled back, so the later genuine
# `use` short-circuited and could not restore them.
#
# rakudo's model, measured: the IMPORTS are lexical to whichever scope ran the
# `use`, while the MODULE and its own state persist process-wide -- which is what
# lets an outer `use` re-import and work.
#
# `Test`'s own `use-ok` is `EVAL ( "use $code" )`, so every `use-ok 'M'; use M;`
# pair hit this. See
# news/2026-09/module-loaded-in-an-eval-keeps-its-imports.md.
#
# `roast/S11-modules/lexical.t` pins the other half of the rule (a block's own
# import must NOT leak), and passes unchanged.

plan 3;

my $dir = $*PROGRAM.parent.add('lib').add('eval-import').Str;

# `EvalImport::Outer` uses `EvalImport::Inner`, which imports `nativecast` from
# NativeCall into its own scope; `outer-probe` reports whether that import is
# still resolvable from inside `Inner`.
is (EVAL "use lib '$dir'; use EvalImport::Outer; 1"), 1,
   'the module loads inside an EVAL';

use lib $*PROGRAM.parent.add('lib').add('eval-import').Str;
use EvalImport::Outer;

is outer-probe(), 'visible',
   "a nested module's own import survives its first load happening in an EVAL";

{
    use EvalImport::Outer;
    is outer-probe(), 'visible', 'a repeat use keeps it resolvable';
}
