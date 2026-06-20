use Test;

# Slice F (env<->locals coherence): `my $tap = do whenever $sup {…}` inside a
# react block binds the tap handle by writing env[target_var] directly (see
# run_whenever_with_value), without updating the caller's local slot. With the
# reverse env->locals pull disabled (single-store default), a read of that
# variable later in the SAME react block saw the stale slot. The whenever scope
# op must reconcile the caller's slots from env after binding. Regression:
# roast/S32-io/IO-Socket-Async.t ('listen tap is a Tap').

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
