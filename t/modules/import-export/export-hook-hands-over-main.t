use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

plan 1;

# Test::Describe: a module whose `sub EXPORT` returns `"&MAIN" => &MAIN` makes
# that routine the importing program's MAIN, run at program end. The module's
# own (non-exported) MAIN candidates used to be dropped before the hook ran, so
# the hook saw `Nil` and no MAIN ran.
is_run ｢use lib 't/lib'; use ExportHookMain; say "mainline"｣,
    %(:out("mainline\nhook main ran\n")),
    'a MAIN handed over by the EXPORT hook runs after the mainline';
