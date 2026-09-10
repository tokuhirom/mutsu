use Test;

# `todo/tickets/procasync-output-chunks-do-not-hold-back-final-grapheme.md`:
# a Proc::Async output Supply never hands out the last grapheme of a chunk. It
# cannot know the grapheme is finished — the next read could start with a
# combining mark that extends it — so it holds it back and flushes it alone once
# the stream ends. And when the stream instead ends in a decode error, the held
# text dies with it rather than being flushed.
#
# Every expectation here was measured against rakudo 2026.06 and is recorded in
# the ticket. The children are all bounded (`sh` exits on its own) and every
# start Promise is awaited, so nothing here depends on a timeout or a port.

plan 12;

# Collect the chunks one stream delivers, in order.
sub chunks-of($proc, $supply) {
    my @chunks;
    react {
        whenever $supply { @chunks.push($_) }
        whenever $proc.start { }
    }
    @chunks;
}

# 1. The canonical boundary case: two writes separated in time, so each lands in
#    its own read(). rakudo: ("ab", "cde", "f").
{
    my $proc = Proc::Async.new('sh', '-c', 'printf "abc"; sleep 1; printf "def"');
    is chunks-of($proc, $proc.stdout), ("ab", "cde", "f"),
        'stdout holds each chunk\'s final grapheme back and flushes it at end of stream';
}

# 1b. Only an EXTENDABLE grapheme is held. UAX #29 GB4 breaks after LF and after
#     any Control unconditionally, so a chunk ending in a newline goes out whole
#     -- which is what keeps line-oriented output streaming. Holding the newline
#     back deadlocks any consumer that replies to a line before the child writes
#     again (`roast/S17-procasync/kill.t`: the child prints "Started" and then
#     blocks reading, so a held-back newline means `.lines` never fires and the
#     kill never happens).
{
    my $proc = Proc::Async.new('sh', '-c', 'printf "Started\n"; sleep 1; printf "more"');
    my @chunks = chunks-of($proc, $proc.stdout);
    is @chunks[0], "Started\n", 'a chunk ending in a newline is delivered whole';

    # The line is visible while the child is still running, not only at exit.
    my $p2 = Proc::Async.new('sh', '-c', 'printf "Started\n"; sleep 30');
    my $seen = '';
    react {
        whenever $p2.stdout.lines { $seen = $_; done }
        whenever $p2.start { }
    }
    is $seen, 'Started', '.lines sees a line as soon as the child writes it';
    $p2.kill('KILL');

    # CR is the exception: the next read may start with the LF that joins it
    # into one grapheme, so it stays held.
    my $p3 = Proc::Async.new('sh', '-c', 'printf "a\r"; sleep 1; printf "b"');
    is chunks-of($p3, $p3.stdout).List.raku, ("a", "\r", "b").raku,
        'a trailing CR is still held back, since LF may follow';
}

# 2. stderr behaves identically — and, crucially, still flushes what it held, so
#    no byte is lost.
{
    my $proc = Proc::Async.new('sh', '-c', 'printf "xyz" >&2');
    my @chunks = chunks-of($proc, $proc.stderr);
    is @chunks, ("xy", "z"), 'stderr holds its final grapheme back too';
    is @chunks.join(''), 'xyz', 'and flushes it, so the whole stream still arrives';
}

# 3. The merged `.Supply` shares the readers' decoders, so it holds back the same
#    way rather than re-assembling whole reads.
{
    my $proc = Proc::Async.new('sh', '-c', 'printf "abc"; sleep 1; printf "def"');
    is chunks-of($proc, $proc.Supply), ("ab", "cde", "f"),
        'the merged Supply inherits the holdback';
}

# 4. The reason for the holdback: a combining mark arriving in the NEXT read must
#    still join the base character it follows. Emitting "ae" eagerly would have
#    cut this grapheme in half.
{
    my $proc = Proc::Async.new('sh', '-c', 'printf "ae"; sleep 1; printf "\314\201z"');
    my @chunks = chunks-of($proc, $proc.stdout);
    is @chunks, ("a", "\x[e9]", "z"),
        'a combining mark from a later read still composes with its base';
    is @chunks.join('').chars, 3, 'and the result is three graphemes, not four';
}

# 5. `\r\n` is a single grapheme, so it is held back as a unit and can never be
#    split across two chunks — which is what keeps mutsu's stdout CRLF rewrite
#    correct when the pair straddles a read boundary.
{
    my $proc = Proc::Async.new('sh', '-c', 'printf "a\r"; sleep 1; printf "\nb"');
    my $got = '';
    react {
        whenever $proc.stdout { $got ~= $_ }
        whenever $proc.start { }
    }
    is $got, "a\nb", 'a CRLF split across two reads is still translated as one pair';
}

# 6. A stream that goes bad discards what it was holding instead of flushing it:
#    the "-" never reaches the tap. (rakudo: got="ok")
{
    my $proc = Proc::Async.new('sh', '-c', 'printf "ok-"; sleep 1; printf "\377\377"');
    my ($got, $quit) = ('', '');
    react {
        whenever $proc.stdout { $got ~= $_; QUIT { $quit = 'quit'; done } }
        whenever $proc.start { }
    }
    is "$got/$quit", 'ok/quit', 'text held back when the stream goes bad is discarded';
}

# 7. With both writes landing in one read(), the whole pending decode is dropped,
#    not just the held grapheme. (rakudo: got="")
#
#    ONE printf, not two. Two `printf` builtins are two write(2) calls, and
#    whether the reader coalesces them into a single read() is a scheduling
#    race, not something the test can assert: if the reader reaches the pipe
#    between the writes it sees "ok-" first, emits "ok", and the case degrades
#    into case 6 above (got="ok"). That is exactly how this file failed once
#    under parallel `make test` load (3/36 concurrent runs reproduced it,
#    `got: 'ok/quit'`). A single printf is one write of 5 bytes -- far under
#    PIPE_BUF -- so the malformed bytes are guaranteed to be in the same read()
#    as the "ok-", which is the condition this case exists to test. Verified
#    against rakudo 2026.06: still "/quit".
{
    my $proc = Proc::Async.new('sh', '-c', 'printf "ok-\377\377"');
    my ($got, $quit) = ('', '');
    react {
        whenever $proc.stdout { $got ~= $_; QUIT { $quit = 'quit'; done } }
        whenever $proc.start { }
    }
    is "$got/$quit", '/quit', 'a read that hits a malformed byte delivers nothing at all';
}

# vim: expandtab shiftwidth=4
