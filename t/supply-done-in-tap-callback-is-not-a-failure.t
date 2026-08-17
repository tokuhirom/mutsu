use Test;
plan 4;

# `done` inside a `whenever` body ends the enclosing supply. It is a control
# signal the supply machinery owns, NOT a failure — so `Supplier.emit` must
# propagate it unchanged rather than route it to the supplier's quit handlers,
# which strips the control flag and re-raises it as a thrown `X::ControlFlow`.
#
# When it did, a `done` that crossed a channel reader thread (the thread that
# drives a `whenever` on a socket supply) surfaced there as an unhandled
# "done without supply or react" and killed the whole process. That aborted the
# vendored Cro suite's `http-middleware.rakutest` mid-file in 2 runs out of 3.

# `done` in a whenever body completes the supply, and does not quit it.
{
    my $source = Supplier.new;
    my $quit-reason;
    my $done = False;
    my $s = supply {
        whenever $source -> $v {
            done if $v eq 'stop';
        }
    }
    $s.tap(-> $ { }, done => { $done = True }, quit => -> $r { $quit-reason = $r });
    $source.emit('one');
    $source.emit('stop');
    ok $done, 'done in a whenever body completes the supply';
    nok $quit-reason.defined, 'done did not reach the quit handler';
}

# The same when the emit originates on a channel reader thread: a `whenever` on
# a socket supply is driven from its own thread, and a `done` raised in a
# downstream whenever body must still complete that downstream supply (and must
# not surface on the reader thread as an unhandled exception).
{
    my $listen-tap = IO::Socket::Async.listen('127.0.0.1', 0).tap(-> $conn {
        $conn.print("bye");
        $conn.close;
    });
    my $port = $listen-tap.socket-port.result;
    my $conn = await IO::Socket::Async.connect('127.0.0.1', $port);

    my $relay = Supplier.new;
    my $upstream = supply {
        whenever $conn.Supply -> $data {
            $relay.emit($data);
        }
    }

    # The downstream tap MUST subscribe to $relay before the upstream tap
    # starts reading the socket and re-emitting: Supplier.emit does not
    # buffer for late subscribers, so if the upstream read (on its own
    # reader thread) raced ahead and emitted before this subscription
    # existed, the emit would be silently lost and $done would never keep —
    # observed as a load-sensitive hang (todo/tickets/supply-done-in-tap-
    # callback-load-flaky.t.md: reproduced 10-25/24-40 runs under heavy CPU
    # contention with the taps in the opposite order, 0/15 with this order).
    my $done = Promise.new;
    my $downstream = supply {
        whenever $relay -> $data {
            done;
        }
    }
    my $down-tap = $downstream.tap(-> $ { }, done => { $done.keep(True) });
    my $up-tap = $upstream.tap(-> $ { });

    await Promise.anyof($done, Promise.in(10));
    ok $done.status ~~ Kept, 'done survives an emit that originated on a reader thread';
    pass 'the process is still alive';

    $down-tap.close;
    $up-tap.close;
    $listen-tap.close;
}
