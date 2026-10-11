use v6;
use Test;

plan 2;

# #12626: a module installed in an `inst#` repository must be scanned at parse
# time like a `-I` one. Its dependency's constants (exported with
# `is export`) are complete nullary terms; unresolved, the importer's
# `($x) ?? IC_RUN !! IC_FLUSH` died with "Your !! was gobbled".

my $r = run($*EXECUTABLE, '-I', 'inst#t/fixtures/installed-const-ternary', '-e',
    'use InstConst; say pick(1), pick(0)', :out, :err);
is $r.out.slurp(:close).trim, '1122', 'ternary over installed-module constants parses and runs';
is $r.exitcode, 0, 'exit code 0';
