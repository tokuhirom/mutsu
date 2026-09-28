use Test;
plan 3;

my $server = IO::Socket::INET.new(:localhost('127.0.0.1'), :localport(0), :listen);
my $sock = IO::Socket::INET.connect('127.0.0.1', $server.localport);
ok $sock.defined, "connect returns a defined value";

my $peer = $sock.getpeername;
ok $peer.defined, "getpeername returns a defined value";
like $peer, /\d+\.\d+\.\d+\.\d+\:\d+/, "getpeername returns ip:port format";
$sock.close;
$server.close;
