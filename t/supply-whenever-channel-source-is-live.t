use Test;
plan 4;

# A `whenever` on a channel-backed live source (an `IO::Socket::Async` listener,
# a connection's incoming byte supply, ...) keeps the enclosing `supply` block
# open. Such a source is driven by a reader thread and only ends when the
# channel signals done, so it must NOT be treated as a finite source that
# completes at tap time.
#
# When it was, every `supply` block sitting on a socket supply fired its
# downstream `done` the moment it was tapped — and with it every `whenever`'s
# LAST phaser up the chain. That tore a whole Cro response pipeline down before
# the first request arrived: `Cro::HTTP::Middleware::Conditional` closes its
# early-response Supplier from `LAST $connection-state.early-responses.done`,
# so an early response (the classic "403 without an Authorization header") was
# never delivered.

# A supply block whose only source is a listener is not done at tap time.
{
    my $listener = IO::Socket::Async.listen('127.0.0.1', 0);
    my $done-fired = False;
    my $s = supply {
        whenever $listener -> $conn {
            emit 'connection';
            $conn.close;
        }
    }
    my $tap = $s.tap(-> $ { }, done => { $done-fired = True });
    nok $done-fired, 'a supply block over a listener is not done at tap time';
    $tap.close;
}

# It still counts as one source among several: the block stays live and keeps
# delivering from its other whenever.
{
    my $listener = IO::Socket::Async.listen('127.0.0.1', 0);
    my $side = Supplier.new;
    my $done-fired = False;
    my @got;
    my $s = supply {
        whenever $side -> $v { emit $v }
        whenever $listener -> $conn { emit 'connection'; $conn.close }
    }
    my $tap = $s.tap({ @got.push($_) }, done => { $done-fired = True });
    $side.emit('still alive');
    nok $done-fired, 'a supply block over a listener and a Supplier is not done at tap time';
    is @got, ['still alive'], 'the supply block still delivers from its other source';
    $tap.close;
}

# ... and the done does arrive once the channel really ends: the server closes
# the connection, so the client's incoming supply signals done, which completes
# the supply block wrapped around it.
{
    my $server-port = Promise.new;
    my $listen-tap = IO::Socket::Async.listen('127.0.0.1', 0).tap(-> $conn {
        $conn.print("hi");
        $conn.close;
    });
    $server-port.keep($listen-tap.socket-port.result);

    my $conn = await IO::Socket::Async.connect('127.0.0.1', $server-port.result);
    my $done = Promise.new;
    my $s = supply {
        whenever $conn.Supply -> $data { emit $data }
    }
    my $tap = $s.tap(-> $ { }, done => { $done.keep(True) });
    ok await(Promise.anyof($done, Promise.in(10))) && $done,
        'the supply block completes once its channel-backed source ends';
    $tap.close;
    $listen-tap.close;
}
