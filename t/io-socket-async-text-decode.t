use Test;

plan 4;

# A real TCP stream splits wherever it likes, so a text-mode `.Supply` has to
# carry decoding state across reads: a read can end mid-UTF-8-sequence or
# mid-grapheme, and emitting either half on its own is wrong.

sub echo-once(*%listen-opts) {
    my $got = Channel.new;
    my $listener = IO::Socket::Async.listen('127.0.0.1', 0, |%listen-opts);
    my $tap = $listener.tap(-> $conn {
        $conn.Supply.tap(-> $chunk { $got.send($chunk) });
    });
    ($tap, await($tap.socket-port), $got);
}

{
    my ($tap, $port, $got) = echo-once();
    my $client = await IO::Socket::Async.connect('127.0.0.1', $port);
    # A base character and its combining mark in two separate packets.
    await $client.write('u'.encode('utf-8'));
    await $client.write("\c[COMBINING DOT ABOVE]\n".encode('utf-8'));
    is $got.receive, "u\c[COMBINING DOT ABOVE]\n",
            'a grapheme split across packets arrives whole';
    $client.close;
    $tap.close;
}

{
    my ($tap, $port, $got) = echo-once();
    my $client = await IO::Socket::Async.connect('127.0.0.1', $port);
    # A multi-byte codepoint split down the middle.
    my $bytes = "пиво\n".encode('utf-8');
    await $client.write($bytes.subbuf(0, 3));
    await $client.write($bytes.subbuf(3));
    my $seen = '';
    $seen ~= $got.receive until $seen.ends-with("\n");
    is $seen, "пиво\n", 'a codepoint split across packets arrives whole';
    $client.close;
    $tap.close;
}

{
    # A socket created with :enc decodes with that encoding, not UTF-8.
    my ($tap, $port, $got) = echo-once(enc => 'latin-1');
    my $client = await IO::Socket::Async.connect('127.0.0.1', $port);
    await $client.write("Öl\n".encode('latin-1'));
    is $got.receive, "Öl\n", 'the listener :enc decodes incoming bytes';
    $client.close;
    $tap.close;
}

{
    # Bytes that are not valid UTF-8 quit the Supply rather than being
    # replaced with U+FFFD.
    my $quit = Channel.new;
    my $listener = IO::Socket::Async.listen('127.0.0.1', 0);
    my $tap = $listener.tap(-> $conn {
        $conn.Supply.tap(-> $chunk { }, quit => { $quit.send('quit') });
    });
    my $port = await $tap.socket-port;
    my $client = await IO::Socket::Async.connect('127.0.0.1', $port);
    await $client.write(Buf.new(0xFF, 0xFF));
    is $quit.receive, 'quit', 'malformed UTF-8 quits a text-mode Supply';
    $client.close;
    $tap.close;
}
