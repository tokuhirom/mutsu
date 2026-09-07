use v6;
use Test;
use lib $*PROGRAM.parent.add('lib');
use MONKEY-SEE-NO-EVAL;

plan 3;

# `use NativeCall` installs two ambient things that are neither the loading
# scope's imports nor `our` package globals: the prelude helper routines
# (`nativesizeof`, `nativecast`, ... registered as `GLOBAL::name` so a body
# under any package can call them by bare name) and the `NativeCall` package
# symbol itself.
#
# Both used to be swept away by a scope restore -- an `EVAL "use ..."` is what
# `Test`'s `use-ok` compiles to -- while `loaded_modules` went on claiming the
# module was loaded, so the program's own later `use` was a no-op that could
# never put them back. `use-ok 'NativeHelpers::Blob'` followed by the real
# `use` left the module's own `BODY_OF` dying with "Unknown function:
# nativecast", and `NativeLibs`' `::('NativeCall')` probe reporting a Failure.

EVAL 'use EvalUseNativeCall::Guts';
use EvalUseNativeCall::Guts;

ok guts-size() > 0,
    'a module first loaded inside an EVAL keeps its NativeCall prelude helpers';

# The package-symbol half: an EVAL that loaded NativeCall must not leave the
# program's own `use NativeCall` a no-op that never re-publishes the symbol.
EVAL 'use NativeCall';
use NativeCall;
ok ::('NativeCall') !~~ Failure,
    'the NativeCall package symbol survives an EVAL that loaded it first';

ok nativesizeof(Pointer) > 0, 'and the prelude helpers still answer';
