use Test;

# A `.tap` on a channel-backed live Supply must not hold a pool worker while
# it is idle (#12892). Each tap used to keep one worker blocked on its channel
# for the tap's whole lifetime, re-checking its close flag every few
# milliseconds, so N tapped connections cost about N pool threads, each waking
# hundreds of times a second. Now a tap's callback runs as a pool task only
# when an event has arrived, the way Rakudo schedules each emitted value.

plan 5;

my constant N = 20;

# The taps of different connections run concurrently.
my $lock = Lock.new;
my $server-bytes = 0;
my $listener = IO::Socket::Async.listen('127.0.0.1', 0);
my $listen-tap = $listener.tap: -> $conn {
    $conn.Supply(:bin).tap: -> $chunk { $lock.protect: { $server-bytes += $chunk.bytes } };
};
my $port = await $listen-tap.socket-port;

my @clients;
for ^N {
    my $c = await IO::Socket::Async.connect('127.0.0.1', $port);
    $c.Supply.tap: -> $ { };
    @clients.push: $c;
}
sleep 0.5;

my $tasks = "/proc/$*PID/task".IO;
sub pool-threads() { $tasks.dir.grep({ ((try .add('comm').slurp.chomp) // '') eq 'pool' }) }
# A pool thread may exit between the listing and the read (an idle worker
# retires); one that is gone counts as zero.
sub wakeups(@threads) {
    [+] @threads.map: {
        my $status = try .add('status').slurp;
        $status ?? ($status ~~ /'voluntary_ctxt_switches:' \s* (\d+)/)[0].Int !! 0
    }
}
if $tasks.d {
    my @pool = pool-threads();
    cmp-ok @pool.elems, '<', N, "{2 * N + 1} idle taps hold fewer than {N} pool threads ({@pool.elems})";
    my $before = wakeups(@pool);
    sleep 1;
    my $woke = wakeups(@pool) - $before;
    cmp-ok $woke, '<', 50, "idle taps do not wake pool threads periodically ($woke wake-ups in 1s)";
}
else {
    skip 'no per-thread accounting on this platform', 2;
}

# Every tap, idle until now, still sees what arrives later.
await $_.write("ab".encode) for @clients;
my $deadline = now + 10;
sleep 0.05 until $lock.protect({ $server-bytes }) >= 2 * N || now > $deadline;
is $lock.protect({ $server-bytes }), 2 * N, 'every server-side tap received its bytes after idling';

# Closing an idle tap stops it: nothing reaches its callback afterwards.
my @after-close;
my $supplier-tap = Supply.interval(0.01).tap: -> $ { @after-close.push: 1 };
sleep 0.1;
$supplier-tap.close;
# A body that had already started when `close` ran may still finish; none may
# start afterwards.
sleep 0.05;
my $seen = @after-close.elems;
sleep 0.2;
is @after-close.elems, $seen, 'a closed tap dispatches nothing more';
ok $seen > 0, 'the interval tap ran before it was closed';

.close for @clients;
$listen-tap.close;
