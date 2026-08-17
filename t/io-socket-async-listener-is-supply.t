use Test;

# `IO::Socket::Async.listen(...)` really IS a Supply in raku (built from a
# `supply { ... }` block in CORE.setting -- there is no separate "Listener"
# type). mutsu implements it as a bespoke native object and its type smart-
# matched False against Supply, so code that branches on
# `$connection-source ~~ Supply` (as IO::Socket::Async::SSL's `!server-setup`
# does) fell through to treating the LISTENER ITSELF as a single accepted
# connection -- see todo/tickets/io-socket-async-listener-supply-method-
# missing.md. Verified against raku directly.

plan 3;

my $listener = IO::Socket::Async.listen("127.0.0.1", 0);
ok $listener ~~ Supply, 'IO::Socket::Async.listen(...) smartmatches Supply';

# Mirror IO::Socket::Async::SSL::!server-setup's own dispatch shape: only on
# the Supply leg is the source tapped per-connection instead of being
# treated as a connection itself.
my $took-supply-leg = False;
my @seen;
if $listener ~~ Supply {
    $took-supply-leg = True;
} else {
    @seen.push($listener.^name);
}
ok $took-supply-leg, 'the Supply leg was taken (matches raku, not the Listener-as-connection fallback)';

# The listener's `tap`/`act` must still be its OWN real (immutable) handler,
# not the generic Supply-mut one that a naive MRO widening would route to via
# an MRO-walk fallback (that generic handler creates a Tap whose
# `socket-port` Promise is never kept, hanging `await $tap.socket-port`
# below -- see call_native_instance_method_mut's explicit exclusion in
# native_methods/mod.rs).
my $tap = $listener.tap(-> $conn { @seen.push($conn.^name) });
my $port = await $tap.socket-port;
my $client = IO::Socket::INET.new(host => '127.0.0.1', port => $port);
sleep 0.2;
$client.close;
$tap.close;

is @seen[0], 'IO::Socket::Async', 'the tapped item is a real per-connection socket, not the listener itself';
