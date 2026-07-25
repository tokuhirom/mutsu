# A `whenever <Promise>` nested inside another `whenever` body now runs

```raku
my $sup = Supplier.new;
my $s = supply {
    whenever $sup.Supply -> $v {
        emit "got-$v";
        whenever Promise.in(0.05) { emit "timer-after-$v" }
    }
}
my @got;
$s.tap: { @got.push($_) };
$sup.emit(1);
sleep 0.3;
say @got.raku;
# raku: ["got-1", "timer-after-1"]
# was:  ["got-1"]
```

The nested subscription was silently dropped. A nested `whenever` on a **Supply**
source always worked, so this was specific to a promise source.

## Root cause

#5409 taught the `supply` block's *own* run to handle a promise source, by
rewriting the subscription marker its body registers. A `whenever` registered
from inside another whenever's body runs later, when the block is already tapped
and there is no emit-buffer frame to register a marker into, so
`run_whenever_with_value` falls through to its non-react arm — which knew how to
subscribe a `Supply` source (via `.tap`) and an
`IO::Socket::Async::Listener` (via `.act`), and nothing else. A promise source
matched neither and fell off the end of the function.

## Fix

That arm now has a promise case, modelled on the `Supply` one next to it: a
promise source is a one-shot supply, so the body runs once with the kept result
followed by the LAST phasers, and a broken promise runs the QUIT phasers instead.
The body runs on whichever thread resolves the promise, so it drives a thread
clone — the same pair `promise_chain_method` uses for `.then`. Its `emit` reaches
the enclosing supply block through the emitter it closed over, exactly as it does
when the body is called as a supplier tap.

Pin: `t/supply-nested-whenever-promise.t` (including two levels of nesting, and
a nested Supply source as the control).

## Still open

`Test::Scheduler` (`TODO_dist` T-037) is not unblocked by this — its remaining
failure turned out to be a different, deeper shape, now recorded as
`todo/deep/cold-supply-whenever-source-replayed-not-tapped.md`: a *cold*
on-demand supply used as a `whenever` source is replayed synchronously rather
than tapped, so the promise subscriptions its body registers are collected as
emitted values. That one no longer needs a virtual scheduler to reproduce.
