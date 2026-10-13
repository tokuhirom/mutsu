use Test;

# Listening sockets are watched by the shared socket reactor, as Rakudo's event
# loop watches them (#12968): opening more listeners adds no threads, every
# listener still accepts, and a closed one frees its port.

plan 5;

my $task-dir = '/proc/self/task'.IO;
# The scheduler's `pool` workers grow lazily (the `await`s below run on the
# pool), so they are not counted: a listener must add no thread of its own
# (#13089).
sub thread-count() {
    $task-dir.dir.grep({ (try .add('comm').slurp.chomp) ne 'pool' }).elems
}

my @taps;
my @ports;
for ^2 {
    my $tap = IO::Socket::Async.listen('127.0.0.1', 0).tap: -> $conn { $conn.print("ok\n"); $conn.close };
    @taps.push: $tap;
    @ports.push: await $tap.socket-port;
}
my $before = $task-dir.d ?? thread-count() !! 0;
for ^6 {
    my $tap = IO::Socket::Async.listen('127.0.0.1', 0).tap: -> $conn { $conn.print("ok\n"); $conn.close };
    @taps.push: $tap;
    @ports.push: await $tap.socket-port;
}
if $task-dir.d {
    is thread-count(), $before, 'six more listeners add no threads';
}
else {
    skip 'no /proc/self/task to count threads';
}

sub fetch($port) {
    my $c = await IO::Socket::Async.connect('127.0.0.1', $port);
    my $got = '';
    react { whenever $c.Supply -> $d { $got ~= $d } }
    $got
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
