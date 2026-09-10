use Test;

plan 5;

# `IO::Socket::Async.connect` must open a real outbound TCP connection when the
# target is not an in-process listener. Previously it only ever looked in the
# in-process listener registry and broke the Promise for anything else, so no
# mutsu program could talk to a server in another process.

# A separate process is the only way to prove the connection is a real socket:
# an in-process listener takes the in-memory path instead. The port travels
# through a file rather than the child's stdout, so the handshake only depends
# on the filesystem.
my $port-file = $*TMPDIR.add("mutsu-real-connect-{$*PID}.port");
my $child = q:to/CODE/;
    my $listener = IO::Socket::Async.listen('127.0.0.1', 0);
    my $tap = $listener.tap(-> $conn {
        $conn.Supply.tap(-> $text { $conn.print("echo: $text") });
    });
    my $port = await $tap.socket-port;
    @*ARGS[0].IO.spurt("$port\n");
    sleep 30;
    CODE

my $server = Proc::Async.new($*EXECUTABLE.absolute, '-e', $child, $port-file.absolute);
my $started = $server.start;

my $port = 0;
for ^300 {
    if $port-file.e {
        my $text = $port-file.slurp.trim;
        if $text {
            $port = $text.Int;
            last;
        }
    }
    sleep 0.1;
}

ok $port > 0, "helper server reported a port ($port)";

my $conn = await IO::Socket::Async.connect('127.0.0.1', $port);
ok $conn.defined, 'connect to an out-of-process listener succeeds';
isa-ok $conn, IO::Socket::Async, 'the kept value is an IO::Socket::Async';

# A real socket has a real OS file descriptor; the in-memory pair reports -1.
ok $conn.native-descriptor > 0, 'a real connection has an OS file descriptor';

my $reply = Promise.new;
react {
    whenever $conn.Supply -> $text {
        $reply.keep($text.trim);
        done;
    }
    whenever start { $conn.print("hello\n") } { }
    whenever Promise.in(20) { done }
}
is ($reply ?? $reply.result !! ''), 'echo: hello',
    'bytes travel over the real connection in both directions';

$conn.close;
$server.kill;
await Promise.anyof($started, Promise.in(5));
$port-file.unlink if $port-file.e;

done-testing;
