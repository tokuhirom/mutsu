use Test;
plan 3;

my $sock = IO::Socket::INET.connect('google.com', 80);
ok $sock.defined, "connect returns a defined value";

my $peer = $sock.getpeername;
ok $peer.defined, "getpeername returns a defined value";
like $peer, /\d+\.\d+\.\d+\.\d+\:\d+/, "getpeername returns ip:port format";
