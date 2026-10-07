use Test;
use Cro::HTTP::Router;
use Cro::HTTP::Server;
use Cro::HTTP::Client;

# The cross-thread store is keyed by bare variable name, so a plain value it
# holds under a name can belong to a different binding of that name. The
# vendored Cro::HTTP stack has `my $response;` inside a `supply { }` body
# (ResponseParser), assigned and read from `whenever` callbacks on a pool
# thread, while the test script declares `my $response = await ...` in a loop.
# The script's in-flight declaration left a plain `Any` placeholder in the
# store; the callback thread's next sync then replaced its own
# `$response` cell with that placeholder, so `$response.http-version = ...`
# died with `No such method ... for invocant of type 'Any'` and the second
# request's awaited value was `Any` (#12204). A binding a thread holds as a
# cell is never replaced by a foreign plain snapshot.
#
# A pure-Raku minimal repro was not found (see
# t/concurrency/promise/await-after-cro-request-keeps-caller-lexicals.t, the
# same class of failure), so this pins the real round trip.

plan 4;

# An unused local port: listen on 0, read the port, release it.
my $probe = IO::Socket::Async.listen('localhost', 0).tap(-> $c { $c.close });
my $port = await $probe.socket-port;
$probe.close;
my $url = "http://localhost:$port";

my $app = route {
    get -> 'hits' { content 'text/plain', 'Visit' }
}
my $service = Cro::HTTP::Server.new(:host('localhost'), :$port, application => $app);
$service.start;
END $service.stop();

my $client = Cro::HTTP::Client.new;
my @names;
my @bodies;
for 1..3 -> $i {
    my $response = await $client.get("$url/hits");
    push @names, $response.^name;
    push @bodies, await $response.body-text;
}
is @names.join(' '), 'Cro::HTTP::Response Cro::HTTP::Response Cro::HTTP::Response',
    'a loop-declared $response keeps the awaited Response on every request';
is @bodies.join(' '), 'Visit Visit Visit', 'every body is readable';

# The same shape with the other names Cro's own code uses for locals.
my @seen;
for 1..2 -> $i {
    my $request = await $client.get("$url/hits");
    my $status = $request.status;
    push @seen, "$i:$status";
}
is @seen.join(' '), '1:200 2:200', '$request and $status survive the awaits too';

my $response = await $client.get("$url/hits");
is $response.^name, 'Cro::HTTP::Response', 'a file-scope $response is intact';
