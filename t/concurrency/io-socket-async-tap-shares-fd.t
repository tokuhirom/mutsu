use Test;

# Tapping an IO::Socket::Async connection's Supply must not duplicate its file
# descriptor (#12981). The reader used to read a `dup` of the socket, so every
# tapped end of a connection held two descriptors where rakudo holds one; a
# keep-alive client making a request per fresh connection ran out of them
# twice as fast.

plan 3;

my constant N = 20;

my $fd-dir = "/proc/$*PID/fd".IO;
sub fds() { $fd-dir.dir.elems }

my $server-bytes = 0;
my $lock = Lock.new;
my $listener = IO::Socket::Async.listen('127.0.0.1', 0);
my $listen-tap = $listener.tap: -> $conn {
    $conn.Supply(:bin).tap: -> $chunk { $lock.protect: { $server-bytes += $chunk.bytes } };
};
my $port = await $listen-tap.socket-port;

# Warm up: the first tap may open the read reactor's own wake pipe.
my $warm = await IO::Socket::Async.connect('127.0.0.1', $port);
$warm.Supply.tap: -> $ { };
sleep 0.3;

my @clients;
if $fd-dir.d {
    my $before = fds();
    for ^N {
        my $c = await IO::Socket::Async.connect('127.0.0.1', $port);
        $c.Supply.tap: -> $ { };
        @clients.push: $c;
    }
    sleep 0.3;
    my $per-pair = (fds() - $before) / N;
    cmp-ok $per-pair, '<=', 2.1, "a tapped connection costs one descriptor per end ($per-pair per pair)";
}
else {
    skip 'no /proc fd listing on this platform';
    for ^N { my $c = await IO::Socket::Async.connect('127.0.0.1', $port); $c.Supply.tap: -> $ { }; @clients.push: $c }
}

# The shared descriptor still reads and writes both ways.
await $_.write("abc".encode) for @clients;
my $deadline = now + 10;
sleep 0.05 until $lock.protect({ $server-bytes }) >= 3 * N || now > $deadline;
is $lock.protect({ $server-bytes }), 3 * N, 'every tapped connection still delivers its bytes';

# Closing a connection ends its tap: the reader sees end of stream.
my $done = Promise.new;
my $c = await IO::Socket::Async.connect('127.0.0.1', $port);
$c.Supply.tap: -> $ { }, done => { $done.keep };
$c.close;
await Promise.anyof($done, Promise.in(5));
ok $done, 'closing a tapped connection completes its Supply';

.close for |@clients, $warm;
$listen-tap.close;
