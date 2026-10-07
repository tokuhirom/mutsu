use Test;
use Cro::HTTP::Router;
use Cro::HTTP::Server;
use Cro::HTTP::Client;

# A native-typed declaration hoisted to a block's entry (`my int $n`, as the
# upstream NativeCall CArray code and the vendored Cro stack have) is that
# block's own binding. A worker running such a method published the seed under
# the bare name `n` while the cross-thread store held the awaiting thread's own
# `$n`, and the next `await` pulled it back over the caller's variable: after
# the first Cro::HTTP::Client request every later read of a same-named
# variable of the caller (`$n`, `$i`) was stale, so the session tests of the
# vendored Cro::HTTP suite compared `Visit $i` against `Visit 0`.
#
# A pure-Raku minimal repro of the cross-thread shape was not found (see
# t/routines/signature/cro-client-nested-param-shadow.t, the same class of
# failure), so this pins the real round trip against the vendored Cro::HTTP.

plan 6;

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
my $n = 0;
my @seen;
for 1..3 -> $i {
    $n++;
    my $reply = await $client.get("$url/hits");
    is await($reply.body-text), 'Visit', "request $i answered";
    push @seen, "$i/$n";
}
is @seen.join(' '), '1/1 2/2 3/3', 'the caller\'s $i and $n survive every await of a request';

my $total = 0;
for 1..3 -> $i {
    my $p = $client.get("$url/hits");
    $total += $i;
    await $p;
    $total += $i;
}
is $total, 12, 'a variable updated either side of an await keeps its value';

my @bodies;
given $client -> $c {
    for 1..2 -> $i {
        given await $c.get("$url/hits") {
            push @bodies, await(.body-text) ~ $i;
        }
    }
}
is @bodies.join(','), 'Visit1,Visit2', 'a loop variable read inside a given after an await';
