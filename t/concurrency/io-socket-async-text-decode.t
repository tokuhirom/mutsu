use Test;

plan 9;

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

# The Supply decodes with the shared streaming decoder (#11783): its errors
# are the decoder's, an incomplete sequence at the end of the stream quits,
# and a socket translates no `\r\n`. Expected values are the `raku` output.
sub run-one(Blob $bytes, *%opts) {
    my $result = Channel.new;
    my $listener = IO::Socket::Async.listen('127.0.0.1', 0, |%opts);
    my $tap = $listener.tap(-> $conn {
        my $text = '';
        $conn.Supply.tap(-> $chunk { $text ~= $chunk },
            done => { $result.send("done:" ~ $text.raku) },
            quit => { $result.send("quit:" ~ .message) });
    });
    my $client = await IO::Socket::Async.connect('127.0.0.1', await $tap.socket-port);
    await $client.write($bytes);
    $client.close;
    my $r = $result.receive;
    $tap.close;
    $r
}
is run-one(Buf.new(0x61, 0xFF, 0x62)), 'quit:Malformed UTF-8 near byte ff',
    'malformed UTF-8 quits with the decoder error';
is run-one(Buf.new(0x61, 0xD0)),
    'quit:Incomplete character near bytes d0 at the end of a stream',
    'an incomplete sequence at the end of the stream quits';
is run-one("a\r\nb".encode), 'done:"a\r\nb"', 'a socket keeps \r\n';
is run-one(Buf.new(0x61, 0xE9), enc => 'ascii'),
    'quit:Will not decode invalid ASCII (code point (233) > 127 found)',
    'an :enc<ascii> socket rejects a high byte';
is run-one(Buf.new(0x61, 0xE9), enc => 'latin-1'), 'done:"aé"',
    'an :enc<latin-1> socket decodes every byte';
