use Test;
use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test::Util;

plan 9;

# `PRE {}`/`POST {}` at the true top-level mainline (outside any sub/method)
# used to be a silent no-op: the condition's `CheckPhaser` opcode was compiled
# but `split_block_phasers` extracted the phaser bodies out of the mainline's
# statement list and discarded them, so the assertion never actually ran (see
# `news/2026-08/pre-post-phasers-enforced-at-mainline.md`). These pin the fix
# via a real subprocess (`is_run`), which exercises the true script mainline
# rather than `EVAL`.

is_run 'PRE { False }; say "reached"',
    { status => sub { 0 != $^a }, out => '', err => rx/'Precondition' .* 'failed'/ },
    'PRE at mainline dies before the next statement runs';

is_run 'PRE { True }; say "reached"',
    { status => 0, out => "reached\n" },
    'PRE at mainline lives when the condition is true';

is_run 'say "before"; POST { False }; say "after"',
    {
        status => sub { 0 != $^a },
        out    => "before\nafter\n",
        err    => rx/'Postcondition' .* 'failed'/,
    },
    'POST at mainline runs after every other statement, then dies';

is_run 'say "before"; POST { True }; say "after"',
    { status => 0, out => "before\nafter\n" },
    'POST at mainline lives when the condition is true';

# A PRE at mainline runs BEFORE every other statement -- even ones textually
# preceding it -- matching real `raku` (verified separately with `raku -e`).
is_run 'say "before"; PRE { False }; say "after"',
    { status => sub { 0 != $^a }, out => '', err => rx/'Precondition' .* 'failed'/ },
    'PRE at mainline runs before statements that textually precede it';

# Statement-form (no braces) PRE/POST at mainline are enforced the same way.
is_run 'PRE 0; say "reached"',
    { status => sub { 0 != $^a }, err => rx/'Precondition' .* 'failed'/ },
    'statement-form PRE at mainline is enforced';

is_run 'say "before"; POST 0',
    { status => sub { 0 != $^a }, err => rx/'Postcondition' .* 'failed'/ },
    'statement-form POST at mainline is enforced';

# Routine-body PRE/POST already worked before this fix -- pin that it is
# unaffected by the mainline change.
throws-like 'sub f { PRE { False }; 1 }; f()', X::Phaser::PrePost,
    phaser => 'PRE', 'PRE inside a sub body is still enforced';

lives-ok { EVAL 'sub f { PRE { True }; POST { True }; 1 }; f()' },
    'satisfied PRE/POST inside a sub body still lives';
