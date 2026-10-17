use Test;

# `IO::Socket::INET`'s methods are rows (ADR-11276 §9.158).

plan 9;

my $server = IO::Socket::INET.new(:localhost('127.0.0.1'), :localport(0), :listen);
ok $server.localport > 0, 'localport of a listening socket';
my $port = $server.localport;

my $client = start {
    my $c = IO::Socket::INET.new(:host('127.0.0.1'), :$port);
    $c.print("one\n");
    $c.put("two");
    $c.write("three\n".encode);
    my $reply = $c.recv;
    $c.close;
    $reply;
};

my $conn = $server.accept;
isa-ok $conn, IO::Socket::INET, 'accept answers a socket';
is-deeply $conn.nl-in.sort, ("\n", "\r\n").sort, 'nl-in: the default separators';
is $conn.get, 'one', 'print, then get';
is $conn.get, 'two', 'put adds the newline';
is $conn.read(6).decode, "three\n", 'write, then read';
$conn.print('done');
is await($client), 'done', 'recv';
ok $conn.close, 'close';
$server.close;

dies-ok { IO::Socket::INET.new(:localhost('127.0.0.1'), :localport(0), :listen).say('x') },
    'IO::Socket::INET has no say of its own, as in Rakudo';
