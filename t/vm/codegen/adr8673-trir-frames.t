# ADR-8673 D1: a call from one TRIR routine to another defers its
# routine-stack entry until an op that can observe the stack runs.
#
# The fixture chains three TRIR routines per observer (a call out of TRIR,
# `nqp::die`, a user `.Bool`, a user `.Str` under `~`, a dynamic variable,
# `EVAL`, a wrapped callee, a callee's exception, a warning) and prints what
# each observer saw. The transcript must be the same with the entries pushed
# lazily, eagerly (`MUTSU_TRIR_LAZY_FRAMES=0`) and with TRIR off, and it is
# pinned against rakudo's.
use Test;

plan 8;

my $fixture = $?FILE.IO.parent(3).add('fixtures/trir-lazy-frames.raku').Str;

sub transcript(%extra-env) {
    my %env = %*ENV;
    %env{$_} = %extra-env{$_} for %extra-env.keys;
    my $proc = run($*EXECUTABLE, $fixture, :out, :err, :%env);
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    ($proc.exitcode, $out, $err)
}

my ($lazy-code, $lazy-out, $lazy-err) = transcript({ MUTSU_VM_STATS => '1' });
my ($eager-code, $eager-out, $eager-err) = transcript({ MUTSU_TRIR_LAZY_FRAMES => '0' });
my ($off-code, $off-out, $off-err) = transcript({ MUTSU_TRIR => 'off' });

is $lazy-code, 0, 'the fixture runs clean with lazy entries'
    or diag "stderr was:\n$lazy-err";
is $eager-out, $lazy-out, 'lazy and eager entries agree';
is $off-out, $lazy-out, 'TRIR off agrees too';

# Checked against rakudo 2026.11.
is $lazy-out, q:to/END/, 'the transcript is what rakudo prints';
    callframe=leaf-call,middle-call,outer-call / leaf-call
    die=boom 1 leaf-die,middle-die,outer-die
    die=boom 2 leaf-die,middle-die,outer-die
    bool-callers=leaf-bool,middle-bool,outer-bool
    bool=yes
    str=leaf-concat,middle-concat,outer-concat
    dynamic=dyn-7
    eval=leaf-eval,middle-eval,outer-eval
    before-wrap=leaf-1
    after-wrap=wrapped(leaf-2)
    throw=thrown 3 throw,thrower,leaf-throw,middle-throw,outer-throw
    warn=4
    pure=42
    END

# The warning's location is a backtrace taken from inside a TRIR routine.
ok $lazy-err.contains("careful 4\n  in sub leaf-warn at "),
    'the warning names the TRIR routine it was raised in';
is $eager-err.lines.grep(*.starts-with('careful') | *.starts-with('  in ')).join("\n"),
    $lazy-err.lines.grep(*.starts-with('careful') | *.starts-with('  in ')).join("\n"),
    'the warning is the same with lazy and eager entries';

# Not vacuous: entries were deferred, and only some had to be pushed.
my $stats = $lazy-err.lines.first(*.contains('trir: entries='));
my $deferred = +($stats ~~ / 'lazy-deferred=' (\d+) /)[0];
my $pushed = +($stats ~~ / 'lazy-pushed=' (\d+) /)[0];
ok $deferred > 0, "inner TRIR calls deferred their entries ($deferred)"
    or diag $stats;
ok 0 < $pushed < $deferred, "observers pushed some of them, not all ($pushed)"
    or diag $stats;
