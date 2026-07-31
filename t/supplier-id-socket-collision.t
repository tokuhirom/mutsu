use Test;

plan 2;

# In-memory async-socket supplies used to stamp their Supply's `supplier_id`
# attribute with an id from the *supply* counter — a separate sequence from
# `Supplier.new`'s supplier counter, both starting at 1. A genuine Supplier
# with the same number then cross-delivered its emissions straight into the
# socket's tap (Cro::TCP messages showing up on a client's .Supply(:bin) tap).
# Spray enough Suppliers that any namespace overlap would cross-deliver.

sub free-port() {
    my $t = IO::Socket::Async.listen('127.0.0.1', 0).tap(-> $c { $c.close });
    my $p = await $t.socket-port;
    $t.close;
    $p
}

my $port = free-port();
my $server-conns = Channel.new;
my $tap = IO::Socket::Async.listen('127.0.0.1', $port).tap({ $server-conns.send($_) });
my $client = await IO::Socket::Async.connect('127.0.0.1', $port);
my @client-got;
$client.Supply(:bin).tap({ @client-got.push($_) });
my $server-socket = $server-conns.receive;

for ^200 {
    my $s = Supplier.new;
    $s.Supply.tap({ ; });
    $s.emit("stray-$_");
}
sleep 0.2;
is @client-got.elems, 0, 'no foreign Supplier emission reached the socket tap';

$server-socket.write('real'.encode('utf-8'));
my $deadline = now + 5;
sleep 0.05 until @client-got || now > $deadline;
is @client-got.head.decode('utf-8'), 'real', 'the socket tap still receives its own bytes';

$client.close;
$tap.close;
