# Cro::TCP::Connector.establish still hangs with real sockets after the pending-adoption race fix

`roast`'s vendored `Cro::Core` test suite (`t/tcp.rakutest`) hangs at test 33
("Response supply emits a TCP message") even after fixing the
`pending_react_subscriptions` vs. `all_done` early-break race in
`drive_react_subscriptions_loop` (see the PR that introduced
`t/react-whenever-promise-nested-whenever-supply.t`). That fix makes the
*synthetic* repro below pass reliably, but the *real-socket* Cro flow still
hangs — this ticket is about the remaining gap.

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

## Leading hypothesis (unconfirmed)

`react_subs` staying at size 1 for the whole run suggests the outer
`establish()` supply's `whenever self.connect(...) -> $transform { ... }`
either (a) never got its own react_subs entry from the promise-whenever
rewrite in this real-socket case, or (b) got one but it was consumed/removed
without the inner `whenever $transform.transformer($incoming)` ever being
adopted into `pending_react_subscriptions` in the first place — i.e. a
*different* bug from the one just fixed, possibly in how the real
`IO::Socket::Async.connect(...).then: { ... Transform.new(...) }` promise
chain resolves (note the extra `.then` transform between connect and the
Transform instance, absent from the fixed synthetic repro, which used a bare
`Promise.in(...).then: { "transform-obj" }` — try a synthetic repro that adds
an intermediate `.then` to isolate this) or in how `Transform.transformer`'s
own `supply { whenever $incoming { whenever $socket.write(...) {} } whenever
$socket.Supply(:bin) -> $data { ... } }` interacts with a **live/real**
`$incoming` supply (`$source`, tapped via `.act()` per Thread 4/6) rather
than a synthetic finite one.

## Suggested next steps

1. Narrow with a synthetic repro that inserts a real `IO::Socket::Async`
   client/server pair (no Cro classes) reproducing the same nesting shape, to
   rule socket I/O in/out as the differentiator.
2. Instrument (env-gated backtrace print, per CLAUDE.md's debugging
   guidelines) `normalize_promise_whenever_markers` /
   `arm_pending_promise_whenevers` / `pending_react_subscriptions.push` call
   sites to see whether the promise-whenever rewrite and the inner-whenever
   registration actually fire in the real-socket run, and in what order
   relative to the `.act()` consumer threads.
3. Check whether `Cro::TCP::Connector.connect`'s extra `.then: { ... }` stage
   (turning the connect Promise's `IO::Socket::Async` result into a
   `Transform.new(:$socket)`) changes which thread resolves the outer
   `establish()` promise, and whether that thread has the right
   `active_supply_emitters` / `pending_react_subscriptions` context when it
   does.

## Status

Blocks the last 2 subtests of `Cro::Core`'s `tcp.rakutest`
("Establishing connection dies before/once service is started/stopped" —
**note**: those two specific subtest *names* are already passing per test 31
in the current run; the actual failure is the *next* subtest, "Response
supply emits a TCP message" at test 33, whose title in the ledger notes
should be corrected). `Cro::Core` is otherwise fully green (8/9 files); this
is the sole remaining blocker for `tcp.rakutest` and thus `Cro::Core` 9/9.
