# Cro::TCP::Connector.establish still hangs: StreamConsumer delivery is per-Interpreter, not cross-thread

`roast`'s vendored `Cro::Core` test suite (`t/tcp.rakutest`) hangs at test 33
("Response supply emits a TCP message") even after fixing the
`pending_react_subscriptions` vs. `all_done` early-break race in
`drive_react_subscriptions_loop` (PR #6124, `t/react-whenever-promise-nested-whenever-supply.t`).
That fix makes the *synthetic* repro below pass reliably, but the
*real-socket* Cro flow still hangs.

**Root cause is now confirmed** (see the "CONFIRMED root cause" section
below): `Interpreter::supply_stream_consumers` — the fast-path delivery list
an on-demand supply's live consumer registers into — is per-`Interpreter`-
instance state, not shared across a `clone_for_thread()` boundary. An `emit`
that happens to run on a cloned interpreter (e.g. inside a
`promise.on_resolve` callback, which always runs on a freshly cloned
interpreter) can't see the original thread's registered consumer, silently
falls back to a different (global tap) delivery path, and the value is lost
if nothing is registered there either.

## Repro

Requires the vendored Cro::Core checkout under
`tmp/cro-work/C_RO_CRO_CORE_*/lib` (see `tmp/cro-work/inc-paths.txt`; recreate
per `tmp/cro-suite-run.sh` if the directory is gone — it's gitignored).

```
use Cro;
use Cro::TCP;
use Test;

constant TEST_PORT = 31388;

class UppercaseTransform does Cro::Transform {
    method consumes() { Cro::TCP::Message }
    method produces() { Cro::TCP::Message }
    method transformer(Supply:D $pipeline) {
        supply {
            whenever $pipeline -> $message {
                emit Cro::TCP::Message.new(
                    data => $message.data.decode('ascii').uc.encode('ascii'));
            }
        }
    }
}

my $source = supply { emit Cro::TCP::Message.new( :data('bbq'.encode('ascii')) ) }

my $listener = Cro::TCP::Listener.new(port => TEST_PORT);
my $loud-service = Cro.compose($listener, UppercaseTransform);
$loud-service.start;

my $responses = Cro::TCP::Connector.establish(port => TEST_PORT, $source);
react {
    whenever $responses -> $message {
        say "GOT: ", $message.data.decode('ascii');
        done;
    }
}
```

`raku` prints `GOT: BBQ` and exits; `mutsu` (both debug and release, after the
pending-adoption fix) prints up through `about to react` and then hangs
forever (`timeout 10` kills it).

Saved as `tmp/tcp-connector-repro.raku` in a working tree at the time of
writing (gitignored — recreate from the snippet above if it's gone).

## What's different from the fixed synthetic case

The synthetic repro in `t/react-whenever-promise-nested-whenever-supply.t`
nests exactly two levels: `supply { whenever <Promise> { whenever <Supply> {
emit } } }`. The real `Cro::TCP::Connector.establish` /
`Cro::TCP::Connector::Transform.transformer` pair is structurally the same
shape (`establish`'s `supply { whenever self.connect(...) -> $transform {
whenever $transform.transformer($incoming) -> $msg { emit $msg } } }`), and a
repro one level deeper — the inner supply itself containing two more
`whenever`s including a nested one (`tmp/connector-establish-repro6.raku`,
mimicking `Transform.transformer`'s `whenever $incoming { whenever
$socket.write(...) {} } whenever $socket.Supply(:bin) -> $data { emit ...;
LAST done }` shape but with synthetic supplies instead of real sockets) —
**also passes** after the fix. So the extra nesting depth alone is not the
differentiator; something about real `IO::Socket::Async` I/O specifically
is.

## gdb findings (debug build, with the pending-adoption fix applied)

`rust-gdb -batch -ex 'run' -ex 'thread apply all bt' -ex kill --args
./target/debug/mutsu -I <core-lib> tmp/tcp-connector-repro.raku`, interrupted
after ~10s with `timeout -s INT`. Six threads:

- **Thread 1** (main): joining the spawned interpreter thread — nothing
  interesting.
- **Thread 2** (the actual react drive loop): blocked in
  `ReactWaker::wait_activity` inside `drive_react_subscriptions_loop`
  (`src/vm/vm_react_subscriptions.rs:701`), called from
  `drive_react_subscriptions_inner` with **`react_subs=Vec(size=1)`**. Only
  one subscription is registered — the top-level `whenever $responses`
  itself. It is idling, waiting for a wakeup that never comes.
- **Thread 3**: `native_socket_async_listener` closure #6
  (`src/runtime/native_methods/socket_async.rs:449`), inside
  `thread::sleep`/`block_quiescent` — the TCP *listener*'s accept-poll loop.
  Expected to be alive and polling; not obviously stuck.
- **Thread 4** and **Thread 6**: both blocked in `run_supply_act_loop`
  (`src/runtime/native_methods/encoding.rs:460`, called from
  `native_supply_mut_methods.rs:594`, the `.act()` native method's consumer
  loop), waiting on `SupplyReceiver::recv()`. Two independent `.act()`
  consumers are alive and idle — worth identifying which supplies these
  belong to (likely `$source`'s tap and/or the listener's `incoming`
  consumer), since `.act()` is a different registration path from the
  `whenever` marker / `pending_react_subscriptions` machinery the recent fix
  touched.
- **Thread 5**: `async_socket_supply_real_tcp` closure #2
  (`src/runtime/native_methods/socket_async_conn.rs:197`), blocked in a
  **blocking `UdpSocket::recv()`** call inside `block_quiescent` — this is
  the *client* connection's read loop (the connector's `Transform.transformer`
  reading `$!socket.Supply(:bin)`). It is parked waiting for the server's
  reply, which — per the listener thread being alive — should have already
  been written back by the server-side pipeline. This looks like the actual
  place the response either never arrives or arrives but never wakes anyone.

## CONFIRMED root cause (gdb breakpoint trace, same debug build)

Set breakpoints at `subtest.rs:415` (the `run_whenever_with_value` react-mode
check), `:437`/`:442` (marker registration), `vm_react_subscriptions.rs:301`
(`adopt_newly_registered_subscriptions`), and `supply_promise.rs:150`
(`normalize_promise_whenever_markers`), each printing `self.supply_emit_buffer`,
`self.react_active`, the `whenever`'s `param` name, and a short backtrace, then
let the program run to the hang. (`rust-gdb -batch -x <script.gdb> --args
./target/debug/mutsu -I <core-lib> tmp/tcp-connector-repro.raku`, script uses
`set logging`/breakpoint `commands` blocks — see method note below on making
this reproducible.)

The trace shows the **outer** `establish()` whenever (`whenever $connection ->
$transform { ... }`, param `"transform"`) registers correctly on the **main**
interpreter (`self=0x7ffff79f9a78`, the one driving the react loop) and gets
rewritten by `normalize_promise_whenever_markers` as expected — this part
matches the synthetic repro and is fine.

But then a **second, distinct** `Interpreter` instance appears
(`self=0x7fffe00f52b0`) running whenevers named `None` (twice — `whenever
$incoming { whenever $socket.write(...) {} }`), `"message"`/`"data"` (the
`UppercaseTransform`/`Cro::TCP::Connector::Transform` bodies) — **all with
`react_active == 0`**, even though this is logically deep inside the react's
own subscription chain. This second instance is `arm_pending_promise_whenevers`'s
`thread_interp = self.clone_for_thread()` (`src/runtime/supply_promise.rs:207`):
the closure passed to `promise.on_resolve` runs on whichever thread resolves
the promise, using this cloned interpreter.

**The bug:** `runtime_thread.rs`'s `clone_for_thread_excluding` constructs the
clone with `supply_stream_consumers: Vec::new()`, `react_active: 0`,
`current_react_waker: None`, `pending_react_subscriptions: Vec::new()`
(`src/runtime/runtime_thread.rs:568-576`) — **all of the react drive loop's
live-delivery state is per-`Interpreter`-instance, not shared/global.** When
`thread_interp` calls `supplier.emit(transform)` on the *outer* promise's
stand-in supplier (`native_supplier_methods.rs:79` `"emit"` handler), the
handler tries `self.try_stream_emit(sid, &value)` first
(`native_supplier_methods.rs:86-90`) — but `try_stream_emit` reads
`self.supply_stream_consumers`, and `self` here is `thread_interp`, whose list
is empty (the *main* thread's list has the real `StreamConsumer` entry, set up
in `build_react_subscriptions`'s on-demand branch, `vm_react_loop.rs:295-301`).
So `try_stream_emit` finds nothing, falls through to the *global* tap-callback
registry (`supplier_emit_callbacks`), and calls whatever tap **is** globally
registered for that supplier id there — which, because `thread_interp.react_active
== 0`, dispatches the whole nested `whenever` chain through
`run_whenever_with_value`'s **"not in react mode" branch**
(`subtest.rs:478+`, the plain `.tap()`-registration path used outside any
react), NOT the `pending_react_subscriptions` path my earlier fix targets.
This runs `Transform.transformer($incoming)`'s entire body — and its own
nested `whenever`s — synchronously on `thread_interp` and its own spawned
sub-threads, all disconnected from the main interpreter's react loop. When the
innermost `whenever $socket.Supply(:bin) -> $data { emit ...; }` finally
`emit`s a `Cro::TCP::Message` up through the `-> $msg { emit $msg }` chain,
that final `emit` reaches `establish()`'s *original* emitter (the same
Arc-shared `Supplier` **value** the main thread registered a `StreamConsumer`
for — the stamped-emitter/`active_supply_emitters` propagation correctly
carries the right emitter identity across the thread clone). But because that
`emit` call also runs on a `clone_for_thread()`'d interpreter (this time one
spawned somewhere in the nested tap-dispatch chain, not necessarily
`thread_interp` itself), `try_stream_emit` again finds nothing locally, and —
unless a global tap happens to be registered for *this* supplier id too — the
value is silently dropped. The main thread's react loop, whose only source of
truth for "did the response arrive" is its own `supply_stream_consumers` /
`react_subs` / waker, never sees anything and blocks forever in
`ReactWaker::wait_activity`.

**In short: live/streaming delivery for an on-demand supply's `StreamConsumer`
only works when the producer's `emit` happens to run on the exact `Interpreter`
instance that registered the consumer. Any `emit` reached via a
`clone_for_thread()` boundary (promise resolution on a background thread,
nested async socket I/O, etc.) falls back to the process-global tap registry,
which is a *different*, only-partially-overlapping consumer set.** The
synthetic repro (`t/react-whenever-promise-nested-whenever-supply.t`) didn't
hit this because its inner supply (`$inner`) is finite and cold — its emit
happens synchronously within the SAME call chain that resolves the promise, on
`thread_interp`, and gets picked up by `normalize_promise_whenever_markers` +
`arm_pending_promise_whenevers`'s *own* supplier-stand-in machinery (which
*does* register a proper sink via `supplier_sinks_register_batch` on the main
thread before arming) — it never needs `try_stream_emit`/`supply_stream_consumers`
at all. The real Cro chain is deeper: TWO chained on-demand supplies
(`establish()`'s outer supply, wrapping `Transform.transformer()`'s inner
supply) where the inner one's consumption happens through the **StreamConsumer**
fast path (`vm_react_loop.rs`'s "Handle on-demand supplies" branch, used when a
`whenever` taps an on-demand supply *directly inside a running react*), not
through the promise-marker rewrite path at all.

## Why this is architecturally deep, not a quick patch

`supply_stream_consumers` (and `react_active`, `current_react_waker`,
`pending_react_subscriptions`) exist per-`Interpreter` because the react drive
loop, its waker, and its subscription list are the state of *one* react
block's execution, normally confined to one thread. Making them visible across
a `clone_for_thread()` boundary needs one of:

- A **global** (process-wide, keyed by `supplier_id`) `StreamConsumer`
  registry, mirroring how the sink/waker registry (`supplier_sinks_register_batch`
  et al.) already works cross-thread — likely the more consistent fix, since it
  makes "does this supplier have a live consumer" a single source of truth
  instead of two (per-instance `supply_stream_consumers` vs. global tap
  registry) that can silently diverge exactly like this.
- Or: never let react-owned drive state exist only on a `clone_for_thread()`
  clone — thread the *original* interpreter's relevant state (or a handle back
  to it) through `arm_pending_promise_whenevers`/`clone_for_thread`, so
  `try_stream_emit` can still reach the real consumer list.

Either direction touches core cross-thread supply plumbing used by every
`start { }`/`Promise.on_resolve`/async-socket callback in the interpreter, so
this needs its own focused investigation + design pass (possibly an ADR per
the project's "large architectural decision" convention), not a one-line
patch under time pressure.

## Suggested next steps

1. Design the fix as a global `supplier_id -> StreamConsumer` (or similar)
   registry replacing/augmenting the per-`Interpreter` `supply_stream_consumers`
   Vec, consistent with how the sink/waker registry already solves the same
   cross-thread problem for `ReactSubscription.supplier_id`. Consider whether
   `try_stream_emit` should simply become another sink-registry consumer
   instead of a separate mechanism.
2. Write a synthetic repro that isolates JUST the StreamConsumer-cross-thread
   gap (two chained on-demand supplies, the inner one's `emit` reached via a
   real `clone_for_thread()` / `start { }` block, no sockets) to iterate on the
   fix without the Cro/socket overhead.
3. Re-run `tmp/tcp-connector-repro.raku` (recreate per the Repro section) and
   `Cro::Core`'s full `tcp.rakutest` once fixed.

## Method note: gdb under this session's load

This investigation hit repeated spurious `rust-gdb`/`pkill` failures (exit 1,
zero output) that turned out to be transient tool/environment flakiness under
heavy concurrent load from other sessions on the shared container (`uptime`
showed load average 13+ on 12 cores at the time) — not real bugs in the gdb
invocation. If this recurs: retry the exact same command once or twice before
concluding it's a syntax problem; avoid `pkill` entirely if it's failing (use
`ps`/`kill` or just skip cleanup, stray processes are otherwise harmless).
`print self.field` (not `self->field`) is the correct rust-gdb syntax for a
`&mut self` receiver. `set logging file X` / `set logging enabled on` +
breakpoint `commands { silent ... continue end}` blocks (via `-x script.gdb`,
not `-ex` on the command line) is far more reliable than trying to interrupt a
hung `run` with `timeout -s INT` when you need output from *before* the hang —
the breakpoints fire deterministically as the program executes, no interrupt
timing race needed.

## Status

Blocks the last 2 subtests of `Cro::Core`'s `tcp.rakutest`
("Establishing connection dies before/once service is started/stopped" —
**note**: those two specific subtest *names* are already passing per test 31
in the current run; the actual failure is the *next* subtest, "Response
supply emits a TCP message" at test 33, whose title in the ledger notes
should be corrected). `Cro::Core` is otherwise fully green (8/9 files); this
is the sole remaining blocker for `tcp.rakutest` and thus `Cro::Core` 9/9.
