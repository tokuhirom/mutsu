# A module first loaded inside a ROUTINE CALL keeps the prelude routines its
# own body calls.
#
# mutsu has no `NativeCall.rakumod` to import from, so it splices NativeCall's
# helper routines into every compunit that calls one, registering each under
# `GLOBAL::` (`PRELUDE_SUB_TRAIT`). A routine call restores the routine
# registry on the way out, and that rollback dropped `GLOBAL::` entries
# wholesale -- including the splice a module's own body received while it
# loaded. `loaded_modules` is never rolled back, so the later real `use` was a
# no-op that could not put it back, and the module's routine died with
# "Unknown function" ever after.
#
# `lives-ok { ... }` is the routine call here: `Test`'s block argument is
# invoked like any other callable. The `EVAL` is only how `use-ok`-shaped code
# loads a module from inside one; the same shape without `EVAL` is fine, which
# is why the rollback -- not the `EVAL` -- is what this pins.
#
# The sibling constraint is that an ordinary import alias must still go out of
# scope with its block (`roast/S11-modules/lexical.t`,
# `roast/S11-modules/require.t` test 10). A prelude splice is not an alias, so
# only the splice is exempted.
use Test;
use lib $?FILE.IO.parent.add('lib').Str;
use NativeCall;

plan 2;

lives-ok { EVAL 'use NativeCallHelperUser; 1' },
    'a module loads from inside a routine call';

use NativeCallHelperUser;

# `NativeCallHelperUser`'s exported routine calls the spliced helper that its
# own body never declares. Type objects are enough to reach the call: it dies
# on the missing routine long before it would touch a real pointer.
lives-ok { cast-through(Str, Pointer) },
    "the module's body still reaches its spliced helper afterwards";
