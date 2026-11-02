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

# `localhost` may resolve to 127.0.0.1 or ::1 depending on the machine, so the
# test checks the SHAPE of the answer (an IP address, never the name given).
sub is-ip(Str $s) { so $s ~~ /^ <[0..9a..fA..F.:]>+ $/ && $s.contains(any('.', ':')) }

ok is-ip($client.peer-host), "the client's peer-host is a numeric address, not 'localhost'";
ok is-ip($client.socket-host), "the client's socket-host is its local numeric address";
ok is-ip(%seen<socket>), "the accepted connection's socket-host is numeric, not 'localhost'";
ok is-ip(%seen<peer>), "the accepted connection's peer-host is numeric";

$client.close;
$tap.close;
