use Test;
use MONKEY-SEE-NO-EVAL;
use lib 't/lib';

# Every `use` of a module with a `sub EXPORT` hook runs the hook, in an
# `EVAL` as at the top level: a repeated `use M; use M` runs it twice
# (#15698). Checked against rakudo.
plan 4;

my $*HOOK-RUNS = 0;
EVAL 'use CountingExportHook;';
is $*HOOK-RUNS, 1, 'one use runs the hook once';

$*HOOK-RUNS = 0;
EVAL 'use CountingExportHook; use CountingExportHook;';
is $*HOOK-RUNS, 2, 'a repeated use runs it twice';

$*HOOK-RUNS = 0;
EVAL 'use CountingExportHook; { use CountingExportHook; }';
is $*HOOK-RUNS, 2, 'a use in a nested block runs it again';

$*HOOK-RUNS = 0;
EVAL 'use CountingExportHook;';
EVAL 'use CountingExportHook;';
is $*HOOK-RUNS, 2, 'each EVAL runs it';
