use Test;

plan 5;

# `whenever IO::Socket::Async.listen(...)` inside a `supply` block, consumed by
# a plain `.tap` (no react loop). The listener's subscription marker used to be
# a 2-element array that only the react event loop understood: the supply-block
# tap path recognises only 4-element markers, so the marker itself leaked to
# the tap as a spurious emission and the whenever body never ran (Cro::TCP
# Listener.incoming, tcp.rakutest test 15).

sub free-port() {
    my $t = IO::Socket::Async.listen('127.0.0.1', 0).tap(-> $c { $c.close });
    my $p = await $t.socket-port;
    $t.close;
    $p
}

class Wrap { has $.socket is required }

my $port = free-port();
my $incoming = supply {
    whenever IO::Socket::Async.listen('127.0.0.1', $port) -> $socket {
        emit Wrap.new(:$socket);
    }
}

my $conns = Channel.new;
my $tap = $incoming.tap({ $conns.send($_) });
my $client = await IO::Socket::Async.connect('127.0.0.1', $port);
my $got = $conns.receive;
ok $got ~~ Wrap, 'whenever body ran and emitted the wrapper object';
ok $got.socket ~~ IO::Socket::Async, 'the accepted socket was bound to the whenever parameter';
nok $conns.poll, 'no spurious emission before it (the old marker leak)';

my $client2 = await IO::Socket::Async.connect('127.0.0.1', $port);
ok $conns.receive ~~ Wrap, 'second connection emitted too';

$client.close;
$client2.close;
$tap.close;
dies-ok { await IO::Socket::Async.connect('127.0.0.1', $port) },
    'closing the outer tap stops the listener';
