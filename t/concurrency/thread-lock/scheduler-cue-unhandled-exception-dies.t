use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

# Code cued on the scheduler that dies, with no :catch and no uncaught_handler,
# ends the program as rakudo's default handler does. It used to be swallowed,
# so anything waiting on the task's result waited forever (ParaSeq's .batch
# hung on a batch that died). Measured against rakudo.

plan 3;

is_run '$*SCHEDULER.cue({ die "boom" }); sleep 1; say "after"',
    { out => '', err => /'Unhandled exception in code scheduled on thread' .* 'boom'/, status => 1 },
    'a cued block that dies ends the program';

is_run '$*SCHEDULER.cue({ die "boom" }, :catch({ say "caught ", .message })); sleep 0.5; say "after"',
    { out => "caught boom\nafter\n", err => '', status => 0 },
    'a :catch handles it';

is_run '$*SCHEDULER.uncaught_handler = -> $e { say "handled: ", $e.message };
        $*SCHEDULER.cue({ die "boom" }); sleep 0.5; say "after"',
    { out => "handled: boom\nafter\n", err => '', status => 0 },
    'a user uncaught_handler replaces the default exit';
