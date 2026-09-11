use v6;
use Test;

# A `signal()` Supply arms the OS handler while a tap is in force and gives the
# signal back to its previous disposition when the last tap goes -- rakudo taps
# the scheduler's signal supply on tap and untaps it on close.
#
# mutsu registered the watcher at `signal()` *call* time and never unregistered
# it (#7917, split out of #7609), so both halves diverged: a signal arriving
# while nothing was tapped was swallowed by a supply nobody could receive from,
# every `signal()` call left a registration behind for the rest of the process
# (1200 of them in `roast/S17-procasync/stress.t`), and re-tapping a supply held
# in a variable delivered nothing at all.
#
# Everything here is measured against `raku`: it catches while tapped, dies of
# SIGUSR1 once the tap is closed, and catches again after a re-tap.

plan 4;

my $dir = $*TMPDIR.child("mutsu-signal-lifetime-{$*PID}");
$dir.mkdir;
END { try { .unlink for $dir.dir; $dir.rmdir } }

my $n = 0;
sub run-child($source) {
    my $file = $dir.child("child-{$n++}.raku");
    $file.spurt($source);
    my $proc = run($*EXECUTABLE, $file.absolute, :out, :err);
    my $out = $proc.out.slurp(:close);
    $proc.err.slurp(:close);
    $out
}

is run-child(q:to/CHILD/).trim, "caught\nalive".trim, 'a tapped signal is caught and the process survives it';
    my $s = signal(SIGUSR1);
    my $t = $s.tap({ say "caught" });
    sleep 0.3;
    shell "kill -USR1 $*PID";
    sleep 0.5;
    say "alive";
    CHILD

# The tap is closed, so the disposition is back to SIG_DFL and SIGUSR1 kills
# the child before it can print "alive".
is run-child(q:to/CHILD/).trim, "caught", 'closing the last tap hands the signal back to its default action';
    my $s = signal(SIGUSR1);
    my $t = $s.tap({ say "caught" });
    sleep 0.3;
    shell "kill -USR1 $*PID";
    sleep 0.5;
    $t.close;
    sleep 0.2;
    shell "kill -USR1 $*PID";
    sleep 0.5;
    say "alive";
    CHILD

is run-child(q:to/CHILD/).trim, "caught\ncaught-again\nalive".trim, 'a signal supply can be tapped again after its tap was closed';
    my $s = signal(SIGUSR1);
    my $t = $s.tap({ say "caught" });
    sleep 0.3;
    shell "kill -USR1 $*PID";
    sleep 0.5;
    $t.close;
    my $again = $s.tap({ say "caught-again" });
    sleep 0.3;
    shell "kill -USR1 $*PID";
    sleep 0.5;
    say "alive";
    CHILD

# The same over a react loop, which is how `roast/S17-procasync/stress.t` uses
# `signal()`: the loop's `whenever` tap goes when the react ends, so the signal
# is back to its default action by the time the loop is over.
is run-child(q:to/CHILD/).trim, "", 'a react loop that ends releases its signal tap';
    for ^20 {
        react {
            whenever signal(SIGTERM) { }
            whenever Promise.in(0) { done }
        }
    }
    sleep 0.3;
    shell "kill -TERM $*PID";
    sleep 0.5;
    say "alive";
    CHILD
