use Test;

plan 4;

# A BARE `.tap()` (outside a react block) on IO::Socket::Async.listen must drive
# the connection callback, and `recv($limit)` must treat $limit as an UPPER
# limit (return up to that many characters without blocking for the full count).
# Covers roast S32-io/socket-recv-vs-read.t tests 1-4.

my $port = 19995;

IO::Socket::Async.listen("127.0.0.1", $port).tap(-> $conn {
    $conn.print('hello world');
    $conn.close;
});

# Give the listener time to bind.
sleep 0.5;

my $client = IO::Socket::INET.new(host => '127.0.0.1', port => $port);

# recv(2): upper limit of 2 characters.
my $first = $client.recv(2);
ok $first.defined, 'bare .tap() drove the callback (client received data)';
ok $first.chars <= 2, "recv(2) respects the upper limit (got {$first.chars} chars)";
is $first, 'he', 'recv(2) returns the first 2 characters';

# recv with a limit larger than what remains returns the available data without
# blocking for the full limit.
my $rest = $client.recv(100);
ok $rest.chars > 0 && $rest.chars < 100,
    "recv(100) returns available data without blocking for 100 (got {$rest.chars})";

$client.close;
