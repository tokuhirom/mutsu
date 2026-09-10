use Test;

plan 3;

# `Supply.lines` on a Supply whose values arrive through a *channel* (the
# shape used by a real TCP socket, e.g. `IO::Socket::Async`'s per-connection
# `.Supply`) used to silently drop every value: `native_supply_dispatch.rs`'s
# `"lines"` arm builds the derived Supply with a fresh `supply_id` and no
# channel of its own (the source's id survives only as an inert
# `parent_supply_id` attribute), and the direct `.tap()` chokepoint
# (`native_supply_mut_methods.rs`) looked up the channel by the derived
# Supply's own `supply_id` — which never has a channel registered under it —
# instead of following `parent_supply_id` back to the source. It also forwarded
# whatever chunk it read straight to the callback with no line-buffering, so
# even a fixed lookup would have truncated a line split across two `write`s.
# See todo/tickets/supply-lines-drops-channel-backed-supplies.md.
#
# A real two-process TCP round trip is the only way to exercise the channel
# path (an in-process Supplier-backed Supply never hits it) — modeled on
# t/io-socket-async-real-connect.t.

my $port-file = $*TMPDIR.add("mutsu-lines-channel-tap-{$*PID}.port");
my $child = q:to/CODE/;
    my $port = @*ARGS[1].Int;
    my $conn = await IO::Socket::Async.connect('127.0.0.1', $port);
    # Split "hello\n" across two writes so a chunk boundary lands mid-line —
    # the carry-over buffering is the part a naive per-chunk split would break.
    await $conn.print("hel");
    sleep 0.2;
    await $conn.print("lo\nworld\n");
    sleep 0.5;
    $conn.close;
    CODE

my $listener = IO::Socket::Async.listen('127.0.0.1', 0);
my @lines;
my $listen-tap = $listener.tap(-> $conn {
    # A direct .tap() on a channel-backed .lines Supply, entirely outside any
    # react/whenever context — the exact chokepoint the bug was in.
    $conn.Supply.lines.tap(-> $l { @lines.push($l) });
});
my $port = await $listen-tap.socket-port;
$port-file.IO.spurt("$port\n");

my $client = Proc::Async.new($*EXECUTABLE.absolute, '-e', $child, $port-file.absolute, $port.Str);
my $started = $client.start;

for ^80 {
    last if @lines.elems >= 2;
    sleep 0.1;
}

is @lines.elems, 2, 'both lines arrive via a direct .tap() on a channel-backed .lines Supply';
is (@lines[0] // ''), 'hello', 'a line split across two writes is reassembled, not truncated';
is (@lines[1] // ''), 'world', 'the following line is unaffected';

$listen-tap.close;
await Promise.anyof($started, Promise.in(3));
$port-file.unlink if $port-file.e;

done-testing;
