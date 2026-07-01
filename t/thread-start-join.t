use Test;

plan 6;

# `.join` on a Thread must block until the thread completes and sync its
# captured-variable mutations back to the parent — same as `.finish`. It must
# NOT be treated as list-`join` (which would stringify the Thread instance to
# "Thread(...)" and return without waiting).

# Scalar captured-outer write propagates through .join.
{
    my $x = 0;
    Thread.start({ $x = 42 }).join;
    is $x, 42, 'captured scalar write from a Thread propagates through .join';
}

# Array captured-outer mutation propagates through .join.
{
    my @a;
    Thread.start({ @a.push(7) }).join;
    is-deeply @a, [7], 'captured array push from a Thread propagates through .join';
}

# .finish behaves the same as .join (both are the thread-join primitive).
{
    my $y = 0;
    Thread.start({ $y = 99 }).finish;
    is $y, 99, 'captured scalar write propagates through .finish too';
}

# .join actually waits: a slow thread's effect is visible right after .join.
{
    my $done = False;
    my $t = Thread.start({ sleep 0.2; $done = True });
    $t.join;
    ok $done, '.join blocks until the (slow) thread finishes';
}

# Lock condition variable: wait/signal across two threads, coordinated by a lock.
{
    my $l = Lock.new;
    my $c = $l.condition;
    my @log;
    my $t1 = Thread.start({
        $l.protect({
            @log.push('ale');
            $c.wait();
            @log.push('stout');
        });
    });
    until $l.protect({ @log.elems }) == 1 { }
    my $t2 = Thread.start({
        $l.protect({
            @log.push('porter');
            $c.signal();
        });
    });
    $t1.join();
    $t2.join();
    is @log.join(','), 'ale,porter,stout', 'Lock condition variable wait/signal works across threads';
}

# List .join is unaffected by the Thread special-case.
{
    is <a b c>.join('-'), 'a-b-c', 'list .join still works';
}
