use Test;

# ADR-17216 slice 3: the routine `.wrap` tables are one world table shared by
# every thread. A wrap or unwrap made on any thread is seen by a call on every
# other, and concurrent wraps of one routine are all kept. Each expectation
# matches `raku`.

plan 8;

# The ADR's repro: a wrap made by EVAL on a worker reaches the main thread.
{
    sub f { "f1" }
    await start { EVAL q[&f.wrap(-> { "wrapped" })] };
    is f(), 'wrapped', 'an EVAL-made wrap on a worker is seen by the main thread';
}

# A plain `.wrap` on a worker.
{
    sub g { "g" }
    await start { &g.wrap(-> { "<" ~ callsame() ~ ">" }) };
    is g(), '<g>', 'a wrap made on a worker is seen by the main thread';
}

# An unwrap on a worker of a wrap made on the main thread.
{
    sub h { "h" }
    my $handle = &h.wrap(-> { "wrapped h" });
    is h(), 'wrapped h', 'wrapped on the main thread';
    await start { &h.unwrap($handle) };
    is h(), 'h', 'an unwrap on a worker is seen by the main thread';
}

# Concurrent wraps of one routine from several workers: none is lost.
{
    sub k { 0 }
    await (^4).map: { start { &k.wrap(-> { 1 + callsame() }) } };
    is k(), 4, 'wraps made by sibling workers are all kept';
}

# A wrap made on the main thread after a worker started is seen by it.
{
    sub m { "m" }
    my $go = Promise.new;
    my $p = start { await $go; m() };
    &m.wrap(-> { "late " ~ callsame() });
    $go.keep;
    is await($p), 'late m', 'a worker sees a wrap made after it was spawned';
}

# The thread that is NOT wrapping has already called the routine (its call
# caches are warm): the wrap made elsewhere still reaches its next call.
{
    sub n { "n" }
    my $go = Promise.new;
    my $ready = Promise.new;
    my $p = start {
        my @before = (^3).map({ n() });
        $ready.keep;
        await $go;
        (@before.join(','), n())
    };
    await $ready;
    &n.wrap(-> { "w" ~ callsame() });
    $go.keep;
    is-deeply await($p), ('n,n,n', 'wn'), 'a warm worker sees a wrap made on the main thread';
}
{
    sub q { "q" }
    my @before = (^3).map({ q() });
    await start { &q.wrap(-> { "w" ~ callsame() }) };
    is q(), 'wq', 'the warm main thread sees a wrap made on a worker';
}
