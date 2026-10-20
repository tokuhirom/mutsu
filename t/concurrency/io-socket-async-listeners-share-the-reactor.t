use Test;

# Listening sockets are watched by the shared socket reactor, as Rakudo's event
# loop watches them (#12968): opening more listeners adds no threads, every
# listener still accepts, and a closed one frees its port.

plan 14;

my $task-dir = '/proc/self/task'.IO;
# This pins mutsu's shared-reactor architecture: its socket service threads
# have `sock-` names. Scheduler workers and `pool-tick` are unrelated to
# listeners. Connections are sent one at a time, and every listener completes
# its handshake BEFORE the next connection or sample: both its accept thread
# and the worker handling that event have run and set their own names. This
# also covers a worker briefly inheriting the reactor's `sock-conn` name.
sub socket-thread-count() {
    $task-dir.dir.grep({ ((try .add('comm').slurp.chomp) // '').starts-with('sock-') }).elems
}

sub fetch($port) {
    # A synchronous client adds no transient async-connect service thread to
    # the process whose listening service threads we are measuring.
    my $c = IO::Socket::INET.new(host => '127.0.0.1', port => $port);
    my $got = $c.get;
    $c.close;
    $got ~ "\n"
}

my @taps;
my @ports;
for ^2 {
    my $tap = IO::Socket::Async.listen('127.0.0.1', 0).tap: -> $conn { $conn.print("ok\n"); $conn.close };
    @taps.push: $tap;
    @ports.push: await $tap.socket-port;
    is fetch(@ports[*-1]), "ok\n", 'a warmup listener completes its handshake';
}
my $before = $task-dir.d ?? socket-thread-count() !! 0;
if $task-dir.d {
    is $before, 1, 'the warm listeners share exactly one socket service thread';
}
else {
    skip 'no /proc/self/task to count threads';
}
for ^6 {
    my $tap = IO::Socket::Async.listen('127.0.0.1', 0).tap: -> $conn { $conn.print("ok\n"); $conn.close };
    @taps.push: $tap;
    @ports.push: await $tap.socket-port;
    is fetch(@ports[*-1]), "ok\n", 'an additional listener completes its handshake';
}
if $task-dir.d {
    is socket-thread-count(), $before, 'six more listeners add no socket service threads';
}
else {
    skip 'no /proc/self/task to count threads';
}

is fetch(@ports[0]), "ok\n", 'the first listener accepts';
is fetch(@ports[*-1]), "ok\n", 'the last listener accepts';

my $closed-port = @ports[*-1];
@taps[*-1].close;
my $refused = False;
try {
    await IO::Socket::Async.connect('127.0.0.1', $closed-port);
    CATCH { default { $refused = True } }
}
ok $refused, 'a closed listener no longer accepts';
is fetch(@ports[1]), "ok\n", 'the other listeners keep accepting';
