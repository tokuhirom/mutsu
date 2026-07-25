use v6;
use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use lib 't/lib';
use Test;

# `exit` terminates the process, so a MAIN must not be dispatched afterwards.
# mutsu ran the MAIN dispatch unconditionally after the mainline, so
# `sub MAIN(Str :$r!) { }; exit 0` printed `Usage:` and exited 2 where raku exits
# 0 silently. The same made `use <dist>; exit 0` useless as a module-load probe:
# every dist exporting a MAIN dispatched it.

use Test::Util;

plan 6;

# A required-parameter MAIN would print usage and exit 2 if dispatched.
is_run 'sub MAIN(Str :$r!) { say "MAIN ran" }; say "mainline"; exit 0',
    { out => "mainline\n", err => '', status => 0 },
    'exit before an undispatchable MAIN skips its usage and keeps the exit code';

is_run 'sub MAIN() { say "MAIN ran" }; say "mainline"; exit 0',
    { out => "mainline\n", err => '', status => 0 },
    'exit skips a MAIN that would otherwise have run';

is_run 'sub MAIN() { say "MAIN ran" }; exit 3',
    { out => '', err => '', status => 3 },
    'and the exit code is preserved';

# Without an exit, MAIN dispatch is unchanged.
is_run 'sub MAIN() { say "MAIN ran" }; say "mainline"',
    { out => "mainline\nMAIN ran\n", status => 0 },
    'a normal program still dispatches MAIN';

is_run 'sub MAIN(Str :$r!) { say "MAIN ran" }; say "mainline"',
    { out => "mainline\n", status => 2 },
    'and an unsatisfiable MAIN still reports usage with exit 2';

# An imported MAIN behaves the same (this is the shape real distributions use).
is_run 'use lib "t/lib"; use ExitMainFixture; say "mainline"; exit 0',
    { out => "mainline\n", err => '', status => 0 },
    'exit also skips an imported MAIN';
