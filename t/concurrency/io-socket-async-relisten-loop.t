use Test;

plan 5;

# A `my $tap` declared in a loop body whose INITIALIZER spawns a worker
# (`.tap` on a listening socket) used to revert to the previous iteration's
# Tap as soon as a connection was accepted: the spawn happened while the
# declaration was still in flight, so the cross-thread store was seeded with
# the binding this `my` was about to shadow and the re-declaration mask was
# dropped, letting the next sync pull that stale value back over the slot.
#
# Consequence: `$tap.close` closed the PREVIOUS round's listener and left the
# current one bound forever — the "a stopped server keeps answering" family.

# Never hardcode a port: ask the OS for one and reuse the number.
my $probe = IO::Socket::Async.listen('localhost', 0);
my $probe-tap = $probe.tap(-> $conn { $conn.close });
my $port = $probe-tap.socket-port.result;
$probe-tap.close;

ok $port > 0, 'got an ephemeral port to re-listen on';

my @seen;
my $rounds-completed = 0;

for ^3 -> $round {
    my $tap = IO::Socket::Async.listen('localhost', $port).tap(-> $conn { $conn.close });
    my $client = await IO::Socket::Async.connect('localhost', $port);
    $client.close;
    @seen.push($tap.WHICH.Str);
    $tap.close;
    $rounds-completed++;
}

is $rounds-completed, 3, 'all three re-listen rounds completed';
is @seen.elems, 3, 'observed one Tap per round';
is @seen.unique.elems, 3, 'each round observed its OWN Tap, not an earlier one';

# The listener really was released each round: a fourth listen on the same
# port still succeeds.
my $again = IO::Socket::Async.listen('localhost', $port);
my $again-tap = $again.tap(-> $conn { $conn.close });
my $last = await IO::Socket::Async.connect('localhost', $port);
$last.close;
$again-tap.close;
pass 'the port is still bindable after the loop';
