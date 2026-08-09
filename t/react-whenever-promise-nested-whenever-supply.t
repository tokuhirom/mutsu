use Test;

plan 1;

# A `whenever <Promise>` nested inside a `supply { }` block is rewritten into
# a supplier-backed stand-in `Supply` (see `normalize_promise_whenever_markers`
# / t/react-whenever-promise-nested-supply.t) so its subscription is driven by
# the ordinary react drive loop. But when that promise-whenever's own body
# registers ANOTHER `whenever` on a plain `Supply` — a common Cro pattern,
# e.g. `Cro::Connector.establish`'s
#   supply { whenever self.connect(...) -> $transform {
#       whenever $transform.transformer($incoming) -> $msg { emit $msg }
#   } }
# — the resolving thread's `supplier.emit(...)` and `supplier.done()` calls
# land in the SAME drive-loop `waker.drain()` batch (they run back-to-back on
# the thread that resolved the promise). That marks the promise-whenever's
# own subscription `done` in the very same iteration the nested `whenever`
# registered itself into `pending_react_subscriptions` — and the drive loop's
# "every known subscription is done" exit check ran before the next
# iteration's top-of-loop `adopt_newly_registered_subscriptions` call could
# adopt it, so the react ended without ever seeing the inner supply's value.
#
# This was deterministic in a release build (the two native calls are fast
# enough to always coalesce into one drain batch) and only "worked" in a slow
# debug build by accident (enough scheduling slop for the two calls to land
# in separate iterations) — see the investigation notes in the fixing PR.
my $connection = Promise.in(0.05).then: { "transform-obj" };
my $inner = supply { emit "hello"; done; }

my $responses = supply {
    whenever $connection -> $transform {
        whenever $inner -> $msg {
            emit $msg;
        }
    }
}

my $got;
react {
    whenever $responses -> $message {
        $got = $message;
        done;
    }
}
is $got, "hello",
    'a whenever nested inside a supply{}-wrapped promise-whenever still delivers its value';
