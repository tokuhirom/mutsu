use v6;
use Test;

plan 4;

# `IO::Socket::INET.new(:host("h:p"))` always splits the host, even when an
# explicit `:port` is also given — rakudo's constructor splits first and only
# *defaults* the port from the suffix (`%args<port> //= $port`). Passing both is
# what HTTP::UserAgent does (`$request.host` is "localhost:8080" while
# `$request.port` is 8080), which was resolved as "localhost:8080:8080".

# A listener on port 0 gets an OS-assigned port; ask it what it got.
my $server = IO::Socket::INET.new(:localhost('127.0.0.1'), :localport(0), :listen);
my $port = $server.localport;
ok $port > 0, 'listener got an ephemeral port';

my $accepted = start { $server.accept };

my $client = IO::Socket::INET.new(:host("127.0.0.1:$port"), :port($port));
is $client.host, '127.0.0.1', 'the host:port suffix is stripped from the host';
is $client.port, $port, 'the explicit port is kept';
$client.close;

my $conn = await $accepted;
ok $conn.defined, 'the connection reached the listener';
$conn.close;
$server.close;
