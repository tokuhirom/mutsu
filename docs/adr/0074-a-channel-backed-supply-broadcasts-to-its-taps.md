# 0074. A channel-backed Supply broadcasts to its taps; the receiver is not an exclusive transfer

Date: 2026-09-07

Status: Accepted (implemented)

## Context

mutsu represents a channel-backed Supply — one whose values arrive from a Rust
producer rather than from Raku code — as a single `mpsc` receiver parked in a
global registry keyed by the Supply's id (`supply_channel_map` in
`src/runtime/native_methods/state.rs`). The producers are the `Proc::Async`
stdout/stderr reader threads and their merged `.Supply`, `IO::Socket::Async`
read streams, `Supply.interval` and the other scheduled pumps, and the signal
watcher.

`take_supply_channel(id)` **removed** the entry from the registry. That made
acquiring the stream an exclusive transfer of ownership: whichever consumer
asked first — the react drive loop, the `.tap()` act-loop pump, a `zip`, a
`.list` materialisation — owned every value on it, and every later consumer of
the same Supply found nothing at all.

Raku Supplies do not work that way. A Supply is a broadcast point: tapping one
twice gives both taps every value that arrives after they tapped.

### What was actually measured (2026-09-07, rakudo v2026.07 vs mutsu at 13d6509d8)

The `todo/deep/` finding this ADR resolves asserted the fault was general to
"every channel-backed Supply", naming `Proc::Async`, the merged `.Supply`,
`IO::Socket::Async` read streams and the scheduled pumps. Re-measuring every
one of those against `raku` before designing anything narrowed it sharply, and
contradicted one of its claims outright:

| # | Shape | raku | mutsu (before) | Verdict |
|---|---|---|---|---|
| 01 | `Proc::Async` `.stdout` tapped twice with `whenever` | both taps get it | first only | **diverged** |
| 02 | merged `$proc.Supply` tapped twice with `whenever` | both | first only | **diverged** |
| 03 | `Supplier.Supply` tapped twice | both | both | already correct |
| 04 | `Supplier.Supply`, second tap added after a value flowed | late tap misses it | same | already correct |
| 05 | `Supply.from-list` tapped twice | both | both | already correct |
| 06 | `supply { }` block tapped twice | both, body runs twice | same | already correct |
| 07 | `Proc::Async` `.stdout` with `.tap` twice (no react) | both | first only | **diverged** |
| 08 | `.Channel` on a Supply | all values | same | already correct |
| 09 | `IO::Socket::Async` connection `.Supply` tapped twice | **one tap wins, nondeterministically** | one tap wins | already correct |
| 10 | `Proc::Async` `.stderr` tapped twice | both | first only | **diverged** |
| 11 | two separate `$proc.stdout` calls, one `whenever` each | both | first only | **diverged** |
| 12 | `Proc::Async` `.stdout` tapped again after the process exited | late tap gets nothing | same | already correct |
| 13 | `$proc.stdout.lines` tapped twice | both | first only | **diverged** |

Two rows change the shape of the problem:

- **Row 09 is not a bug.** Rakudo does *not* fan out an `IO::Socket::Async`
  connection's read Supply either — the two taps compete and one wins, with
  which one winning varying between runs. mutsu already matches (it just picks
  the winner deterministically). The finding's claim that "a socket read stream
  behaves the same way" was reporting agreement as divergence.
- **Rows 03-06, 08 already fan out.** The `Supplier`-backed, `from-list`,
  `supply { }`-block and `.Channel` paths were all correct. The registry's
  exclusive-transfer defect reaches only the supplies that are *fed by a Rust
  producer thread through the channel registry*, and in practice today that is
  the `Proc::Async` output family.

So the real defect is narrower than reported, but the *mechanism* at fault —
`take_supply_channel` removing the entry — is shared by every channel-backed
Supply, including the socket, timer and signal sources that happen not to be
tapped twice in any test today. Fixing the mechanism fixes the family.

### Live versus on-demand is the axis that constrains the design

Rows 04, 06 and 12 pin the distinction, and they pin it on *both* sides:

- A **live** Supply (`Supplier`-backed; `Proc::Async` output; sockets; timers)
  has one producer running independently of who is listening. A tap that
  registers late sees only what is emitted after it registered — row 04 and
  row 12 both show values emitted earlier are simply gone, in raku and in
  mutsu alike.
- An **on-demand** Supply (`supply { }`, `Supply.from-list`) runs its producer
  once *per tap*. Row 06 measures the body running twice for two taps.

This rules out the obvious naive fan-out. Buffering every value and replaying
the buffer to a late tapper would make live supplies behave like on-demand
ones, breaking rows 04 and 12 — which mutsu currently gets *right*. Any design
here must be a broadcast whose subscribers start empty at the moment they
subscribe, never a replay log.

## Decision

**A channel-backed Supply's registry entry is a broadcast point, not a
transferable receiver.** `take_supply_channel` stops removing the entry and
instead returns a fresh, independent **subscriber** attached to it. Every
subscriber has its own queue and sees every event distributed after it
subscribed, and nothing before.

Concretely, in `src/runtime/native_methods/supply_channel.rs`:

- The registry holds a `SupplyReceiver` acting as a *template*: it carries the
  shared broadcast state, and its own queue is registered **lazily, on first
  poll**, so it accumulates nothing while it sits unclaimed. Laziness rather
  than a queue-less template is what makes this safe: several sites
  (a `whenever <Promise>`'s one-shot channel, the signal watcher, the socket
  listeners) never park their handle in the registry at all and poll it
  directly, and they must keep behaving like an ordinary consumer. A handle
  registers its queue before it pumps, so the upstream backlog still reaches
  it.
- `take_supply_channel(id)` clones that handle with a newly registered queue
  and hands it back. The template stays in the registry, so a second, third or
  Nth consumer gets an equally complete stream.
- **A producer retires when its last tap goes away.** Dropping the sole
  consumer used to disconnect the upstream mpsc, because the consumer *was* the
  receiver; that is how `Supply.interval` learned to stop (`register_interval`
  retires its deadline-heap entry when `send` fails). With the registry keeping
  a template alive the mpsc never hangs up on its own, so `send` now reports
  `SendError` once the broadcast has had at least one subscriber and all of
  them have been dropped. Losing the last tap is exactly when Raku stops a live
  Supply's producer. Before the first subscriber exists the producer keeps
  sending, so a `Proc::Async` reader thread that starts before the react loop
  subscribes does not lose its first chunk.
- Distribution is **pull-driven, with no broker thread**: whichever subscriber
  polls first drains the upstream `mpsc` under a `try_lock` and clones each
  event into *every* currently-registered subscriber queue, then pops its own.
  A subscriber that never polls costs nothing to run; a subscriber that is
  dropped is pruned (queues are held by `Weak`), so its memory is released.
- Blocking `recv`/`recv_timeout` are the same loop plus a bounded condvar wait,
  so a missed wakeup costs a few milliseconds of latency rather than a hang.
- `Disconnected` is reported per subscriber, once the upstream has hung up
  *and* that subscriber has drained its own queue — so a late-registered
  subscriber on a finished stream retires immediately, which is what row 12
  requires.

The `.tap()` pump on `Proc::Async` output correspondingly stops serving only
the first registered tap callback (`get_supply_taps(sid).into_iter().next()`)
and runs one act loop per registered tap, each with its own subscriber.

### Why not the alternatives

- **One receiver plus a per-tap cursor over a retained buffer.** This is the
  design that forces a replay log to exist, and a log is exactly what rows 04
  and 12 forbid: a cursor positioned at "now" is indistinguishable from a
  private queue, and a cursor positioned anywhere earlier is wrong. The log
  can only be trimmed to the slowest cursor, so one stalled tap pins the entire
  history of a long-running stream. It buys nothing over per-subscriber queues
  and costs unbounded retention plus a live-semantics hazard.
- **Make tap registration itself the subscription** — i.e. push the fan-out up
  into `Supplier`, so a channel-backed Supply becomes a supplier fed by a
  channel, reusing the sink registry that rows 03/04 prove already correct.
  This is attractive as a unification and remains the long-term shape, but it
  changes the identity and lifetime of every channel-backed source and touches
  all ten `take_supply_channel` consumers, several of which (the `Proc::Async`
  replay guard above all) are built on ownership transfer being observable. The
  broadcast decided here is a strict prerequisite for it and, by keeping
  `take_supply_channel`'s signature, needs no change at any consumer.

## Consequences

- **Not removing the registry entry removes two implicit signals**, both of
  which had to be restored explicitly, and both of which roast caught rather
  than review: "the mpsc has hung up, so the producer should retire" (now the
  last-tap-gone rule above — without it `Supply.interval` ticked forever and
  `roast/S17-supply/syntax.t` hung), and "the handle from
  `supply_event_channel` is somebody's actual receiver" (now lazy queue
  registration — without it every `whenever <Promise>` silently received
  nothing, because its one-shot channel never goes through the registry). The
  general lesson: the previous design overloaded *ownership of the receiver*
  with several unrelated meanings, and each has to be re-expressed on its own
  terms once ownership stops being exclusive.
- Acquiring a channel is no longer an exclusive transfer, so it can no longer
  be *used* as one. The `Proc::Async` await-time replay guard does not depend
  on removal — it depends on `mark_supply_live_tapped`, which the first take
  still sets — so it is unaffected. `has_supply_channel` now answers "this
  Supply is channel-backed" rather than "the channel is still unclaimed";
  its one caller consults it at `whenever`-registration time, before any take.
- **An unconsumed tap costs memory proportional to what it does not consume.**
  A subscriber that registers and then never polls queues every event for as
  long as it is alive. This is bounded in practice by the subscriber being
  dropped (the queue is pruned) and by streams being finite, but a tap held
  open on an infinite source and never polled is an unbounded queue. That is
  the accepted price of correct live fan-out without a broker thread; the
  alternative designs pay it too, and the buffer design pays it globally rather
  than per-tap.
- The shared close flag is still shared: `Tap.close` on one of several taps of
  the same channel-backed Supply stops the source for all of them. That is
  strictly better than the previous behaviour (where the other taps received
  nothing in the first place), but it is not right. Per-subscriber close, and
  retiring the upstream only once every subscriber has closed, is deliberately
  left out of this decision.
- **Not every channel-backed source is a broadcast point, so exclusivity became
  an explicit property.** Row 09 is the measured counter-example: rakudo hands
  each chunk of an `IO::Socket::Async` connection's read Supply to exactly one
  of its taps. Broadcasting it made mutsu deliver to both whenever the second
  tap registered before the first bytes arrived — deterministic-looking when
  idle, and a `make test` failure under load. A source can therefore call
  `SupplyReceiver::mark_exclusive`, and `take_supply_channel` keeps the old
  exclusive-transfer behaviour for it (remove the entry, first asker owns the
  stream). Only the socket connection read Supply sets it today. This is the
  live-versus-competing distinction made explicit rather than inherited from
  whichever mechanism the registry happened to use, which is the honest shape:
  "is this Supply a broadcast point?" is a property of the source, and every
  source now answers it deliberately.
