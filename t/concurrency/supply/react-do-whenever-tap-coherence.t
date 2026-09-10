use Test;

# `do whenever` leaves the Tap on the ordinary value stack, so a later read of
# the lexical in the same react block sees the value assigned by `my` without
# any env-to-local reconciliation. Regression: roast/S32-io/IO-Socket-Async.t
# ('listen tap is a Tap').

plan 2;

{
    my $listen-socket = IO::Socket::Async.listen('0.0.0.0', 0);
    react {
        my $listen-tap = do whenever $listen-socket -> $socket { … }
        ok $listen-tap.defined, "listen tap is defined";
        isa-ok $listen-tap, Tap, "do-whenever bound tap is visible as a Tap in-block";
        done;
    }
}
