use Test;

# `kill` delivers the signal with kill(2) itself rather than spawning a `kill`
# binary from $PATH, so it must keep working when $PATH is empty.

plan 4;

my $exe = $*EXECUTABLE.absolute;
my $proc = Proc::Async.new($exe, '-e', 'sleep 30');
my $pid;
my $p = $proc.start;
$pid = await $proc.pid;

%*ENV<PATH> = '';
ok kill(0, $pid), 'signal 0 probes a live child with PATH empty';
ok kill(SIGTERM, $pid), 'SIGTERM is delivered with PATH empty';
my $res = await $p;
is $res.signal, SIGTERM.value, 'the child died of the signal we sent';
nok kill(0, $pid), 'signalling the reaped child fails';
