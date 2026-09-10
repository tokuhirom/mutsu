use Test;

plan 5;

# A loopback `IO::Socket::Async.connect` is a REAL TCP connection, not an
# in-memory socket pair: both ends must have an OS file descriptor. NativeCall
# consumers depend on it -- Cro::TCP::NoDelay hands `.native-descriptor` to
# `setsockopt(TCP_NODELAY)` and dies when it is not a real fd.

my $accepted = Channel.new;
my $listener = IO::Socket::Async.listen('127.0.0.1', 0);
my $tap = $listener.tap(-> $conn {
    $accepted.send($conn.native-descriptor);
});
my $port = $tap.socket-port.result;
ok $port > 0, 'the listener reports the port the OS assigned';

my $client = await IO::Socket::Async.connect('127.0.0.1', $port);
my $client-fd = $client.native-descriptor;
ok $client-fd > 0, "the connecting end has a real file descriptor ($client-fd)";

my $server-fd = $accepted.receive;
ok $server-fd > 0, "the accepted end has a real file descriptor ($server-fd)";
isnt $client-fd, $server-fd, 'the two ends are distinct descriptors';

$client.close;
$tap.close;

# Connecting to a port nobody listens on must break the Promise, even though
# this process has other listeners.
dies-ok { await IO::Socket::Async.connect('127.0.0.1', 1) },
        'connecting to a closed port fails';
