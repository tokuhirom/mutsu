use Test;

# `IO::Socket::INET` has no `say` of its own: `$sock.say(...)` is `Mu.say`,
# which gists and joins its arguments, adds the newline and writes them through
# the socket's `print` (Rakuish's agent protocol client).

plan 2;

my $server = IO::Socket::INET.new(:listen, :localhost<127.0.0.1>, :localport(0));
my $port = $server.localport;
my $reader = start {
    my $conn = $server.accept;
    my @lines = $conn.get, $conn.get;
    $conn.close;
    @lines
};
my $client = IO::Socket::INET.new(:host<127.0.0.1>, :$port);
$client.say("hello", 42);
$client.say([1, 2]);
$client.close;
my @got = await $reader;
is @got[0], 'hello42', 'the arguments are joined and sent with a newline';
is @got[1], '[1 2]', 'an argument is sent as its gist';
$server.close;
