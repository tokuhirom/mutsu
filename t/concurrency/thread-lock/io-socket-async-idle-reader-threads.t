use Test;

# Reading an IO::Socket::Async connection must not take a thread per
# connection: one reactor thread watches every socket and reads the readable
# ones (#12865). Before, each tapped connection kept its own `sock-conn`
# reader thread waking every 50 ms, so a keep-alive client or server piled up
# one polling thread per idle connection.

plan 3;

my constant N = 20;

my @received;
my $listener = IO::Socket::Async.listen('127.0.0.1', 0);
my $listen-tap = $listener.tap: -> $conn {
    @received.push: start { $conn.Supply.list.join('') };
};
my $port = await $listen-tap.socket-port;

my @clients;
for ^N {
    my $c = await IO::Socket::Async.connect('127.0.0.1', $port);
    $c.Supply.tap: -> $ { };
    @clients.push: $c;
}
await $_.print("x") for @clients;
sleep 0.5;

my $tasks = "/proc/$*PID/task".IO;
if $tasks.d {
    my $readers = $tasks.dir.grep({ .add('comm').slurp.chomp eq 'sock-conn' }).elems;
    cmp-ok $readers, '<=', 1, "{2 * N} tapped sockets are read by at most one reader thread ($readers)";
}
else {
    skip 'no per-thread names on this platform';
}

# Every connection, idle since its first write, still delivers later bytes.
await $_.print("yz") for @clients;
.close for @clients;
my $deadline = now + 10;
sleep 0.05 until @received.elems == N || now > $deadline;
my @texts = await Promise.anyof(Promise.allof(@received), Promise.in(10)).then({ @received.grep(*.status == Kept).map(*.result) });
is @texts.elems, N, 'every connection reached the end of its stream';
is-deeply @texts.unique.List, ("xyz",), 'each delivered all of its bytes, including those sent after idling';

$listen-tap.close;
