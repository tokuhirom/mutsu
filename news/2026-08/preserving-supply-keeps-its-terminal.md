# A finished `Supplier::Preserving` still replays, and `Promise($supply)` waits for a live source

A `Cro::HTTP::Client` response arrived with a correct status line and headers but
an empty body (`todo/tickets/cro-client-response-body-never-arrives.md`). Two
independent supply bugs were stacked on top of each other; both are general
`Supplier`/`Supply` semantics, and both now match Rakudo.

## `Supplier.done` threw the preserved backlog away

`Supplier.done` ended with `supplier_reset(sid)` — "reset the supplier state
after done/quit so it can be reused" — which cleared `emitted`, the `done` flag
and the quit reason. For a plain `Supplier` that is harmless: Rakudo delivers
nothing at all to a tap made after `done`, and a wiped registry entry delivers
nothing either.

For `Supplier::Preserving` it is fatal. Rakudo backs it with a single `@!replay`
list that buffers everything emitted while nobody is listening, and `done`
appends the terminal event to that same list, so:

```raku
my $p = Supplier::Preserving.new;
$p.emit(1); $p.emit(2); $p.done;
$p.Supply.tap: { say "got $_" }, done => { say "done" };   # got 1, got 2, done
$p.Supply.tap: { say "late $_" }, done => { say "late done" };  # nothing at all
```

mutsu printed nothing for either tap. The reset is now skipped for a preserving
supplier, which keeps both halves of that replay list alive, and the terminal
event is tracked as part of it: `done` delivered to a tap that was already
listening marks it delivered (later taps then see nothing, as above), while a
`done` that arrived with nobody listening is claimed by exactly one later tap —
including a `whenever`, whose `LAST` phaser now runs on a source that finished
before it subscribed.

This is what emptied the Cro body. `Cro::HTTP::ResponseParser` funnels the raw
body bytes through a `Supplier::Preserving` precisely because the response object
reaches the client before the body does; the parser had already emitted the five
bytes and `done`d when `body-blob` finally tapped, so the whole body was gone.

## `Promise($supply)` treated a live `Supplier` source as a finite one

The `Promise($supply)` path built real subscriptions for channel-backed sources
and replayed everything else as a *static* source: run the body over whatever the
source had emitted so far, then run its `LAST` phaser. A live `Supplier`-backed
`Supply` has no channel of its own — it pushes through the supplier registry's
sinks — so it took that static path and the promise was kept before the producer
had emitted anything:

```raku
my $p = Supplier.new;
my $s = $p.Supply;
start { sleep 0.2; $p.emit(Buf.new(1,2,3)); $p.done }
say (await Promise(supply {
    my $joined = Buf.new;
    whenever $s { $joined.append($_); LAST emit $joined }
})).elems;
```

Rakudo answers 3; mutsu answered 0, having run `LAST` immediately — before the
`start` block had emitted at all. Such a source now becomes a supplier-backed
`ReactSubscription`, exactly as `react { whenever $supplier.Supply { … } }`
builds, so the drive loop waits for the real `done`.

With both fixed, a mutsu `Cro::HTTP::Client` completes a full round trip against
a mutsu `Cro::HTTP::Server` in the same process and reads the body:
`await $resp.body-text` gives `world`. In the vendored Cro::HTTP suite
`t/http-request-parser.rakutest` reaches 266/311 and
`t/http-response-parser.rakutest` 163/170.

Pins: `t/supplier-preserving-done-replay.t`,
`t/promise-of-supply-live-supplier.t`.
