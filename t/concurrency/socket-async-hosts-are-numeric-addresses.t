use v6;
use Test;

plan 4;

# `socket-host` and `peer-host` of an async TCP socket are the numeric address
# of the connected socket (getsockname / getpeername), not the host name the
# caller gave to `listen` / `connect` (#16029). Listen on port 0 and read the
# assigned port from the tap; never hardcode a port.

my $server-side = Promise.new;
my $tap = IO::Socket::Async.listen('localhost', 0).tap(-> $conn {
    $server-side.keep(%( socket => $conn.socket-host, peer => $conn.peer-host ));
    $conn.close;
});
my $port = await $tap.socket-port;

my $client = await IO::Socket::Async.connect('localhost', $port);
my %seen = await $server-side;

is $client.peer-host, '127.0.0.1', "the client's peer-host is the numeric address, not 'localhost'";
is $client.socket-host, '127.0.0.1', "the client's socket-host is its local numeric address";
is %seen<socket>, '127.0.0.1', "the accepted connection's socket-host is numeric, not 'localhost'";
is %seen<peer>, '127.0.0.1', "the accepted connection's peer-host is numeric";

$client.close;
$tap.close;
