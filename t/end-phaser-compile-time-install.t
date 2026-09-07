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

plan 19;

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

# --- what an unreached END's body actually SEES -------------------------
#
# rakudo's END is a closure that was never CLONED against a live frame, so
# every `my`/`state` lexical it mentions reads as that container's UNASSIGNED
# value -- `Any` for a `$`, an empty `Array`/`Hash` for `@`/`%` -- no matter
# which enclosing scope declared it, and no matter what that scope later
# stored there. mutsu seeds exactly those names into the pre-installed
# phaser's env (`Interpreter::preinstall_end_phaser`); everything that is NOT
# a per-frame lexical container still resolves against the live exit-time env.

is_run 'if False { my $x = 1; END { say $x.^name; say $x.raku } }
say "mainline";
',
    { out => "mainline\nAny\nAny\n", err => '', status => 0 },
    "an unreached END reads its block's `\$` lexical as Any, not Nil";

is_run 'my $t = 5;
if False { END { say $t.raku } }
say "mainline";
',
    { out => "mainline\nAny\n", err => '', status => 0 },
    'and an OUTER lexical too -- the whole frame chain was never instantiated';

is_run 'my @arr = 1, 2, 3;
if False { END { say @arr.raku } }
say "mainline";
',
    { out => "mainline\n[]\n", err => '', status => 0 },
    'an assigned outer Array reads as the empty container';

is_run 'my $w = 1;
if False { my $w = 2; END { say $w.raku } }
say "mainline";
',
    { out => "mainline\nAny\n", err => '', status => 0 },
    "the phaser's own declaration shadows a live same-named outer variable";

is_run 'sub outer() { my $m = 1; if False { my $n = 2; END { say $m.raku, " ", $n.raku } } }
outer();
say "mainline";
',
    { out => "mainline\nAny Any\n", err => '', status => 0 },
    'a routine that DID run still leaves its unreached END nothing to see';

# Everything that is not a per-frame lexical container resolves normally.
is_run 'sub sayit() { say "sub-ran" }
our $pkg = 7;
constant K = 11;
class CL { method m() { "meth" } }
if False { END { sayit(); say $pkg.raku; say K.raku; say CL.new.m; say $*PROGRAM-NAME.defined } }
say "mainline";
',
    { out => "mainline\nsub-ran\n7\n11\nmeth\nTrue\n", err => '', status => 0 },
    'an unreached END still reaches subs, `our`, constants, classes and dynamics';

# A top-level END is ALWAYS reached and closes over the still-live unit scope,
# so it sees the LIVE values -- including writes made after its own declaration,
# and by a later END. It must not be seeded.
is_run 'my $hist;
END { say $hist.raku }
END { $hist ~= "End " }
$hist ~= "main ";
',
    { out => "\"main End \"\n", err => '', status => 0 },
    'a top-level END still sees the live unit lexicals, not unassigned ones';

# mutsu answers `Any` for an uncalled routine's parameter. rakudo answers
# `VMNull` there -- a raw NQP null whose `.defined` throws
# `X::Method::NotFound ... for invocant of type 'VMNull'` -- which is an
# artifact of its binder rather than a Raku value, so this row deliberately
# pins mutsu's answer and not rakudo's.
is_run 'sub g($p) { my $q = 2; END { say $p.^name, " ", $q.^name } }
say "mainline";
',
    { out => "mainline\nAny Any\n", err => '', status => 0 },
    "an uncalled routine's parameter reads as Any (rakudo says VMNull; see the comment)";

# The eager installation must not cost the existing guarantee that an END runs
# when the mainline dies before reaching it.
is_run 'say "start"; die "boom"; if False { END { say "dead-branch" } }; END { say "after-die" }',
    { out => "start\nafter-die\ndead-branch\n", status => 1 },
    'both a mainline and a dead-branch END still run when the body dies';

# An `our` variable is a PACKAGE symbol, installed when the compunit is
# compiled -- so its slot exists (undefined) even when the declaration sits in
# a branch that never runs, and a never-reached END that mentions it sees the
# type object rather than an unbound name.
is_run 'if False { our $o = 4; END { say $o.^name } }
say "mainline";
',
    { out => "mainline\nAny\n", err => '', status => 0 },
    'an our declared in a dead branch is still installed for a never-reached END';

is_run 'if False { our $o = 4 }
say OUR::<$o>.^name;
',
    { out => "Any\n", err => '', status => 0 },
    'and the package symbol itself exists, undefined';
