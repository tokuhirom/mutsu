# Chronic's `at` no longer hangs: on-demand producers stay live, and `my $tap = ….tap({ $tap.close })` works

`Chronic` 0.0.14's `t/040-at.t` never finished under mutsu. Rakudo passes it 4/4 in 6.35s
([#9493](https://github.com/tokuhirom/mutsu/issues/9493)). `Chronic.at($datetime)` returns a
Promise, and it keeps that Promise from a tap on its one-per-process clock supply:

```raku
$supply = Supply.on-demand(-> $p {
    Supply.interval(1).tap({ $p.emit(DateTime.now.truncated-to('second') but DateTimeMatcher) });
});
...
my $tap = self.supply.grep({ $_ == $datetime }).tap({
    $tap.close;
    $v.keep($_);
});
```

Three separate bugs in that code each kept the Promise from being kept. The file now passes
4/4 in 6.4s, and all six Chronic test files pass.

## 1. A `Supply.on-demand` producer finished its tap as soon as it returned

mutsu lowers a `supply { }` block to `Supply.on-demand(-> $emitter { … })`, and the tap path
treated both forms the same way: run the body, deliver what it emitted, then fire `done`. That
is correct for a supply block, which is done once its body and its `whenever`s finish. It is
wrong for an explicit producer, which is done only when it calls `$p.done`. A producer that
returns and then emits later (from a `Supply.interval` tap, from a `start` block) had its tap
completed at once, and nothing was subscribed to hear the later emits.

The tap now tells the two cases apart by the parser's supply-block mark on the callback
(`is_supply_block_producer`). When an explicit producer returns without calling `done` or
quitting, the tap subscribes to the producer's emitter and registers `done` there.

## 2. `.grep` / `.map` over an on-demand source took a snapshot

`Supply.grep` and `Supply.map` ran the source's producer once, at the moment `.grep` or `.map`
was called, and built a static supply from whatever it had emitted synchronously. Against a
live producer that snapshot is empty and already finished. It was also wrong for finite
sources: the producer ran before anything tapped the derived supply, and only once for any
number of taps.

Rakudo defines both methods as `supply { whenever self -> \value { … } }`. They now return an
on-demand supply of the same shape. Its producer is a native `__SupplyDerive` shim that, each
time the derived supply is tapped, taps the source with emit/done/quit forwarders bound to the
derived supply's own emitter. Closing the derived tap also closes the source tap. Every
consumer that already handles an on-demand supply (tap, react `whenever`, `.list`,
`.Promise`) handles the derived supply with no special case. The synthesized "callable whose
body is one native method call" idiom that this shim uses already existed in three copies.
It now lives once, in `native_methods::native_shim`.

## 3. A closure in its own declaration's initializer got no cell

In `my $tap = $s.tap({ $tap.close })`, the closure is created inside `$tap`'s initializer,
before `$tap` is bound. The existing self-capture rule (`self_capture_decl_locals`, written for
`my $f = -> $n { $f($n - 1) }`) marks such a local as mutated after capture. Two checks still
kept it from getting a shared cell:

- the escape analysis boxes only captures of **escaping** closures, and it counts a call
  argument as invoked immediately, which `.tap` does not do;
- the emit-time slot bake (`declared_at_emit`) saw no slot for `$tap` yet, because the
  declaration binds its slot only after it compiles the initializer, so it read the capture as
  a later sibling `my`.

In a single thread, a by-name lookup hid both problems. When the tap callback ran on the
interval's worker thread, the lookup resolved against that thread's environment, and `$tap`
read `Any`. `$tap.close` then died, and `$v.keep` never ran. `compute_free_vars` now always
gives a self-captured declaration its cell and exempts it from the emit-time bake. The closure
is one of the initializer's own, so its capture can only be this declaration.

Pinned by `t/concurrency/supply/supply-on-demand-producer-async-emit.t` and
`t/routines/closure/closure-self-capture-call-arg-cross-thread.t`.
