# A live Supplier's emit now waits for reacts on other threads

`$supplier.emit(...)` into a `react` running on another thread used to push the value onto the
react's queue and return at once, so code right after the `emit` never saw what the `whenever`
did:

```raku
my @got;
my $s = Supplier.new;
my $ready = Promise.new;
start react { whenever $s.Supply -> $m { @got.push($m) }; $ready.keep };
await $ready;
$s.emit($_) for 1..3;
say +@got;   # was 0, now 3 -- as in raku
```

Rakudo runs a live supply's taps synchronously on the emitting thread. mutsu keeps the `whenever`
body on the react's own thread and has the producer wait for it instead: `emit`, `done` and `quit`
return only once every react on another thread that taps the supplier has handled the event. A
`whenever` that already ran in the react body holds the supplier until the react's event loop is
wired up, so an emission made in between waits as well. A handler that blocks (an `await`, a
`Lock`, an `emit` into a react that is waiting on it) releases the producer instead of
deadlocking with it. After the wait the producer sees the handler's writes to shared variables.

Log::Dispatch's `t/010`, `t/030` and the first subtest of `t/040` now pass. The design is
[ADR-11268](../../docs/adr/11268-live-supplier-emit-waits-for-react-handlers.md).
