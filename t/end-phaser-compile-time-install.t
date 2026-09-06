use Test;

# rakudo installs EVERY `END` in a compunit when that compunit is COMPILED, in
# source order, and runs them in reverse. Two things follow that "register when
# execution reaches the declaration" cannot produce:
#
#   * an END inside a block that never runs — or a sub/method that is never
#     called — still runs at exit;
#   * the run order is reverse SOURCE order with no tie between ENDs that share
#     one physical line, because the key is a per-declaration index rather than
#     a line number.
#
# Every case runs in a child process, since an END's whole point is that it
# runs at exit. Expected outputs below were measured against rakudo 2026.07.

use lib $?FILE.IO.parent(2).add('roast/packages/Test-Helpers/lib').Str;
use Test::Util;

plan 9;

is_run 'if False { END { say "never-run-block" } }
sub g      { END { say "uncalled-sub" } }
for 1..3   { END { say "loop" } }
END        { say "main" }
',
    { out => "main\nloop\nuncalled-sub\nnever-run-block\n", err => '', status => 0 },
    'an END in a never-entered block or an uncalled sub still runs, in reverse source order';

is_run 'class C { method m { END { say "in-method" } } }
if False { if False { END { say "deeply-nested" } } }
my $c = do { END { say "in-do" }; 5 };
say "mainline";
END { say "last-decl" }
',
    { out => "mainline\nlast-decl\nin-do\ndeeply-nested\nin-method\n", err => '', status => 0 },
    'a method of an instantiated-nowhere class, and a doubly-nested dead branch, both install';

# The residual the source-LINE ordering key could not fix: several ENDs on one
# physical line. No fixed tie-break is right in both directions, which is why
# the key is now a per-declaration index handed out as the parser walks past.
is_run '{ END { say 1 } }; { END { say 2 } }; END { say 3 }',
    { out => "3\n2\n1\n", err => '', status => 0 },
    'three ENDs on one physical line still run in reverse source order';

is_run 'END { say 3 }; { END { say 1 } }',
    { out => "1\n3\n", err => '', status => 0 },
    'and the opposite arrangement on one line orders the other way round';

# An END is a closure over its declaring scope, and rakudo's is over the frame
# of the LAST execution that reached the declaration -- not the first.
is_run 'sub f($n) { my $v = $n * 10; END { say $v } }
f(1); f(2); f(3);
say "mainline";
',
    { out => "mainline\n30\n", err => '', status => 0 },
    'an END in a sub called repeatedly closes over the last call, and runs once';

is_run 'for 1..3 -> $i { END { say $i } }
say "mainline";
',
    { out => "mainline\n3\n", err => '', status => 0 },
    'an END in a loop body runs once, over the final iteration';

# The declaring block's lexicals exist for an END that was never reached; they
# are simply undefined, because nothing ever assigned them.
is_run 'if False { my $x = 1; END { say $x.defined } }
say "mainline";
',
    { out => "mainline\nFalse\n", err => '', status => 0 },
    "an unreached END sees its block's lexicals as undefined";

is_run 'if False { my @a; END { @a.push(1); say @a.elems } }
say "mainline";
',
    { out => "mainline\n1\n", err => '', status => 0 },
    'and a never-run declaration of a container still gives the END an empty one';

# The eager installation must not cost the existing guarantee that an END runs
# when the mainline dies before reaching it.
is_run 'say "start"; die "boom"; if False { END { say "dead-branch" } }; END { say "after-die" }',
    { out => "start\nafter-die\ndead-branch\n", status => 1 },
    'both a mainline and a dead-branch END still run when the body dies';
