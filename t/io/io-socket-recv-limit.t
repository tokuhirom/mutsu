use Test;

plan 7;

# A BARE `.tap()` (outside a react block) on IO::Socket::Async.listen must drive
# the connection callback, and `recv($limit)` must treat $limit as an UPPER
# limit of complete CHARACTERS: return up to that many characters without
# blocking for the full count, keep multibyte characters intact, and leave any
# excess buffered for the next recv. Covers roast S32-io/socket-recv-vs-read.t
# tests 1-4 and guards the IO-Socket-INET.t multibyte recv.

# A 3-byte character (U+A001) exercises "recv(1) returns 1 whole char".
my $payload = 'he' ~ chr(0xA001) ~ 'llo';

# Port 0 = let the OS assign a free ephemeral port, and ask the tap which one it
# got. A hardcoded port made this test collide with a sibling under `prove -j4`
# (whichever bound second died), which is what the old "flaky" label was.
my $tap = IO::Socket::Async.listen("127.0.0.1", 0).tap(-> $conn {
    $conn.print($payload);
    $conn.close;
});
my $port = await $tap.socket-port;

my $client = IO::Socket::INET.new(host => '127.0.0.1', port => $port);

# recv(2): upper limit of 2 characters.
my $first = $client.recv(2);
ok $first.defined, 'bare .tap() drove the callback (client received data)';
is $first, 'he', 'recv(2) returns the first 2 characters';
is $first.chars, 2, 'recv(2) yields exactly 2 characters';

# recv(1) of the multibyte char returns the whole character, not a partial byte.
my $mb = $client.recv(1);
is $mb, chr(0xA001), 'recv(1) returns a whole multibyte character';
is $mb.chars, 1, '... which is 1 character';

# recv with a limit larger than what remains returns the available data without
# blocking for the full limit, and does not lose the earlier-buffered bytes.
my $rest = $client.recv(100);
is $rest, 'llo', 'recv(100) returns the remaining buffered characters';
ok $rest.chars < 100, 'recv(100) did not block for the full limit';

$client.close;
