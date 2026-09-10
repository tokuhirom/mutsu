use Test;

plan 5;

# Port 0 means "let the OS assign a free ephemeral port" (Raku semantics), and
# `.socket-port` must report the port the kernel actually handed out.
#
# mutsu used to substitute a port from a PROCESS-LOCAL counter that started at
# the same number in every process and checked only its own listener map for
# occupancy. So every mutsu process picked the same port, two concurrent ones
# collided, and whichever bound second died. That is what made the socket tests
# look "flaky under -j4" — it was a deterministic collision, not timing noise.

my $tap = IO::Socket::Async.listen("127.0.0.1", 0).tap(-> $conn {
    $conn.print("first");
    $conn.close;
});
my $port = await $tap.socket-port;

ok $port ~~ Int,  'socket-port on a port-0 listener returns an Int';
ok $port > 1024,  'the OS assigned a real (non-privileged) port';
isnt $port, 0,    '... and not the 0 we asked for';

# A second port-0 listener must land somewhere else: the kernel does not hand
# out a bound port twice. (The old counter returned the same number to every
# process, so this is the property that actually broke.)
my $tap2 = IO::Socket::Async.listen("127.0.0.1", 0).tap(-> $conn {
    $conn.print("second");
    $conn.close;
});
my $port2 = await $tap2.socket-port;
isnt $port2, $port, 'a second port-0 listener gets a different port';

# The reported port is the one that actually accepts connections.
my $client = IO::Socket::INET.new(host => '127.0.0.1', port => $port);
is $client.recv, 'first', 'the reported port is the one that is listening';
$client.close;

# vim: expandtab shiftwidth=4
