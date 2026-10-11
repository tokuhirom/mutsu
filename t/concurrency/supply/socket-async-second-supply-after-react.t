use Test;

plan 3;

# A connection is only read while a tap listens: after a first `react` over
# `$conn.Supply` has finished, the next chunk belongs to the next tap (#12750).
my $listener = IO::Socket::Async.listen('127.0.0.1', 0);
my $port-p = Promise.new;
my $tap = $listener.tap(-> $c {
    $c.Supply.lines.tap(-> $line {
        # Reply in three separate writes, like a handshake plus queued events.
        $c.print("one\n");
        $c.print("two\n");
        $c.print("three\n");
    });
});
my $port = await $tap.socket-port;
my $conn = await IO::Socket::Async.connect('127.0.0.1', $port);

my $first = '';
react {
    whenever $conn.Supply { $first ~= $_; done }
    whenever Promise.in(0.5) { done }
}
is $first, '', 'nothing arrives before the client speaks';

await $conn.print("go\n");
my @lines;
react {
    whenever $conn.Supply.lines {
        @lines.push: $_;
        done if @lines == 3;
    }
    whenever Promise.in(10) { done }
}
is-deeply @lines, ['one', 'two', 'three'], 'the second tap sees every line';
is $first, '', 'the finished first react consumed nothing';

$conn.close;
$tap.close;
