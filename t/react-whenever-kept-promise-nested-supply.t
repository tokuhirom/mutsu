use v6;
use Test;

plan 3;

# Regression test for the arm-vs-sink-registration race
# (todo/deep/stream-consumer-delivery-not-cross-thread-safe.md, the real cause
# of Cro::TCP::Connector.establish hanging at "Response supply emits a TCP
# message").
#
# A `whenever <Promise>` nested in a `supply { }` body is rewritten to a
# stand-in supplier, and the promise is armed afterwards. When the promise is
# ALREADY RESOLVED at arm time, the arm closure fires synchronously: its
# emit+done used to hit the stand-in before the react drive loop had
# registered any sink for it, and the `done` handler's supplier state reset
# wiped the buffered value — the sink replay then found nothing, the whenever
# body never ran, and the react hung forever. Arming must happen only after
# the drive loop's sinks are registered.

# Case 1: already-kept promise guarding a nested live-supplier whenever.
{
    my $sup = Supplier.new;
    my $ready = Promise.new;
    $ready.keep('go');
    my $outer = supply {
        whenever $ready {
            whenever $sup.Supply -> $v {
                emit "w-$v";
            }
        }
    }
    my $got = '';
    my $p = start { sleep 0.5; $sup.emit(42); }
    react {
        whenever $outer -> $v { $got = $v; done; }
    }
    is $got, 'w-42', 'nested whenever behind an already-kept promise still delivers';
}

# Case 2: already-kept promise whose body emits directly (emit+done race on
# the stand-in supplier itself).
{
    my $ready = Promise.new;
    $ready.keep('now');
    my $outer = supply {
        whenever $ready -> $v {
            emit "got-$v";
            done;
        }
    }
    my @vals;
    react {
        whenever $outer -> $v { push @vals, $v; }
    }
    is @vals.join(','), 'got-now', 'already-kept promise whenever body emit is not lost';
}

# Case 3: promise kept later (the non-racy ordering) still works.
{
    my $sup = Supplier.new;
    my $outer = supply {
        whenever start { 'go' } {
            whenever $sup.Supply -> $v {
                emit "w-$v";
            }
        }
    }
    my $got = '';
    my $p = start { sleep 0.5; $sup.emit(7); }
    react {
        whenever $outer -> $v { $got = $v; done; }
    }
    is $got, 'w-7', 'late-resolving promise ordering still delivers';
}
