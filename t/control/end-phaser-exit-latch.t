use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

# Rakudo latches the process status at the *first* `exit`. An `exit` raised
# while one is already unwinding -- an END phaser's own `exit` in a program
# that already said `exit N` -- still ends the block it runs in, but it neither
# overwrites the status nor stops the END phasers that have not run yet.

plan 6;

is_run 'END { say "A"; exit 7 }; exit 42;',
    { :out("A\n"), :42status },
    'an END phaser cannot overwrite the status of an exit already in flight';

is_run 'END { say "A"; exit 7 }; say "main";',
    { :out("main\nA\n"), :7status },
    'an END phaser does set the status of a program that ends on its own';

is_run 'END { say "A1"; exit 7; say "A2" }; exit 42;',
    { :out("A1\n"), :42status },
    'the status-inert exit still ends the phaser body';

is_run 'END { say "A" }; END { say "B"; exit 7 }; exit 42;',
    { :out("B\nA\n"), :42status },
    'an END phaser exit does not skip the END phasers still to run';

is_run 'END { say "A" }; END { say "B"; exit 7 }; say "main";',
    { :out("main\nB\nA\n"), :7status },
    'the remaining phasers run after an exit that does set the status';

is_run 'END { say "A"; exit 3 }; END { say "B"; exit 7 }; say "main";',
    { :out("main\nB\nA\n"), :7status },
    'the first exit wins when two END phasers both exit';
