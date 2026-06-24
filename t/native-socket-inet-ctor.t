use Test;

# Pin for the VM-native `IO::Socket::INET.new` fast path (ledger §D ③ ctor). The
# VM gate in `vm_call_method_compiled.rs` and the interpreter's `dispatch_new`
# arm both call the single `dispatch_socket_inet_new` impl, so the native and
# slow paths must stay byte-identical. The real bind/connect writes only
# VM-owned `io_handles` state (same shape as the native `IO::Path.open`).

plan 7;

# A listening server socket on an ephemeral port.
my $server = IO::Socket::INET.new(:listen, :localhost<127.0.0.1>, :localport(0));
isa-ok $server, IO::Socket::INET, 'server socket constructs';

# Invalid port is rejected at construction.
dies-ok { IO::Socket::INET.new(:host<127.0.0.1>, :port(99999)) },
    'out-of-range port dies';

# Invalid family is rejected at construction.
dies-ok { IO::Socket::INET.new(:host<127.0.0.1>, :port(80), :family(42)) },
    'unsupported family dies';

# A full client/server round-trip over the loopback interface.
my $port-promise = Promise.new;
my $listener = IO::Socket::INET.new(:listen, :localhost<127.0.0.1>, :localport(0));
my $accepted = start {
    my $conn = $listener.accept;
    my $got = $conn.recv;
    $conn.print("echo:$got");
    $conn.close;
    $listener.close;
    $got;
};

# Connect a client and exchange a message.
my $client = IO::Socket::INET.new(:host<127.0.0.1>, :port($listener.localport));
isa-ok $client, IO::Socket::INET, 'client socket constructs';
$client.print("hello");
my $reply = $client.recv;
is $reply, 'echo:hello', 'round-trip reply received';
$client.close;
is (await $accepted), 'hello', 'server received the client message';

# A second independent socket pair is not aliased to the first.
my $l2 = IO::Socket::INET.new(:listen, :localhost<127.0.0.1>, :localport(0));
isnt $l2.localport, $listener.localport, 'independent listeners bind distinct ports';
$l2.close;
