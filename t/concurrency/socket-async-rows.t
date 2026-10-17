use Test;

# `IO::Socket::Async`'s instance methods are rows (ADR-11276 §9.158), for a
# TCP connection and a UDP socket alike.

plan 9;

my $got = Promise.new;
my $server = IO::Socket::Async.listen('127.0.0.1', 0).tap(-> $conn {
    $conn.Supply.lines.tap(-> $line {
        $got.keep($line);
        $conn.print("echo:$line\n");
    });
});
my $port = await $server.socket-port;

my $client = await IO::Socket::Async.connect('127.0.0.1', $port);
is $client.peer-port, $port, 'peer-port';
is $client.peer-host, '127.0.0.1', 'peer-host';
ok $client.socket-port > 0, 'socket-port';
ok $client.native-descriptor > 0, 'native-descriptor';
my $reply = Promise.new;
$client.Supply.lines.tap(-> $line { $reply.keep($line) });
await $client.print("hi\n");
is await($got), 'hi', 'print reaches the peer';
is await($reply), 'echo:hi', 'Supply receives';
dies-ok { $client.print-to('127.0.0.1', $port, 'x') }, 'print-to is for a UDP socket';
$client.close;
$server.close;

my $udp = IO::Socket::Async.bind-udp('127.0.0.1', 0);
isa-ok $udp.Supply, Supply, 'a UDP socket\'s Supply';
$udp.close;
pass 'UDP close';
