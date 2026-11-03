use Test;

# A `react` `whenever $conn.Supply` that closes its tap must stop listening:
# the next chunk stays in the kernel buffer for the next `.Supply` tap.
# Taken from IO::Socket::Async::SSL t/upgrade.rakutest, where the server's
# closed plain-text tap swallowed the TLS ClientHello that the upgraded
# connection's own tap was meant to read.

plan 3;

my $port = Promise.new;
my $first = Promise.new;
my $second = Promise.new;
start {
    react {
        my $lt = do whenever IO::Socket::Async.listen('localhost', 0) -> $conn {
            my $t = do whenever $conn.Supply -> $msg {
                $first.keep($msg.chomp);
                $t.close;
                # The client answers as soon as it has this, so its next chunk
                # can arrive before the re-tap below exists: it must wait in
                # the kernel buffer, not be read by the closed tap's reader.
                $conn.print("go\n");
                # Re-tap from inside a nested supply block, as
                # IO::Socket::Async::SSL.upgrade-server does.
                my $again = supply {
                    $conn.Supply(:bin).tap: -> $b { $second.keep($b.decode.chomp) };
                    whenever Promise.in(30) { }
                };
                whenever $again -> $x { }
            }
        }
        $port.keep(await $lt.socket-port);
        whenever Promise.in(20) { done }
    }
}

my $client = await IO::Socket::Async.connect('localhost', await $port);
await $client.print("hello\n");
my $reply = Promise.new;
my $rt = $client.Supply.tap: -> $m { $reply.keep($m.chomp) };
await Promise.anyof($reply, Promise.in(10));
$rt.close;
await $client.print("again\n");

await Promise.anyof($second, Promise.in(10));
is await($first), 'hello', 'the first tap got the first chunk';
is $reply.status ~~ Kept ?? $reply.result !! 'timeout', 'go', 'the server answered';
is $second.status ~~ Kept ?? $second.result !! 'timeout', 'again',
    'a tap made after the react whenever was closed gets the next chunk';
