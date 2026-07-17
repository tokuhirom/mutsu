use v6;
use Test;

# Pin for the shared deadline-heap timer (PLAN §6, ADR-0008 follow-up):
# pending Promise.in / Promise.at / cue(:in) timers wait on one process-wide
# driver thread, not one sleeping OS thread each. Before the heap, 100
# pending Promise.in timers held 100 OS threads alive for their full delay.

plan 4;

# None of these ever fire during the test (long delays); they must not
# consume a thread each while pending.
my @p;
@p.push: Promise.in(600) for ^50;
@p.push: Promise.at(now + 600) for ^50;
$*SCHEDULER.cue({ ; }, :in(600)) for ^50;

my $threads = "/proc/$*PID/status".IO.lines.first(*.starts-with('Threads:'));
my $n = $threads.words[1].Int;
ok $n < 40, "150 pending timers don't hold OS threads (got $n)"
    or diag "Threads: $n — pending timers are consuming a thread each again";

is @p.grep(*.status eq 'Planned').elems, 100, 'all long-delay promises still Planned';

# The timers must still actually fire: short delays resolve promptly.
my $fired = Promise.in(0.05);
await Promise.anyof($fired, Promise.in(5));
is $fired.status, 'Kept', 'short Promise.in fires while long timers pend';

# A cued callback with :in still runs.
my $p = Promise.new;
$*SCHEDULER.cue({ $p.keep(42) }, :in(0.05));
await Promise.anyof($p, Promise.in(5));
is $p.result, 42, 'cue(:in) callback ran off the shared timer';
