# A channel-backed Supply now broadcasts to every tap

A Supply in Raku is a broadcast point: tapping one twice gives both taps every
value that arrives after they tapped. mutsu represented a channel-backed
Supply — one fed by a Rust producer rather than by Raku code — as a single
`mpsc` receiver parked in a global registry keyed by the Supply's id, and
`take_supply_channel` **removed** it. Acquiring the stream was therefore an
exclusive transfer of ownership: whichever consumer asked first owned every
value on it, and every later consumer of the same Supply found nothing at all.

```raku
my $proc = Proc::Async.new("echo", "two");
my $s = $proc.stdout;
my ($x, $y) = ('', '');
react { whenever $s { $x ~= $_ }; whenever $s { $y ~= $_ }; whenever $proc.start { } };
say "x=", $x.raku, " y=", $y.raku;
# raku:  x="two\n" y="two\n"
# mutsu: x="two\n" y=""          <-- the second whenever got nothing
```

The registry entry is now a **broadcast point** rather than a transferable
receiver ([ADR-0074](../../docs/adr/0074-a-channel-backed-supply-broadcasts-to-its-taps.md)).
`take_supply_channel` no longer removes it; it registers a fresh, independent
subscriber with its own queue and hands that back, so the second, third and Nth
consumer each get the whole stream. Distribution is pull-driven and needs no
broker thread: whichever subscriber polls first drains the upstream `mpsc` under
a `try_lock`, clones each event into every registered queue, then serves itself.
Queues are held by `Weak`, so a dropped tap is pruned and its backlog released.
On top of that, the `Proc::Async` `.tap()` act-loop pump stopped serving only
the first registered tap callback and now runs one act loop per tap.

## Ownership was carrying two other meanings

Removing the registry entry had quietly encoded two signals besides "who owns
the stream", and both had to be re-expressed once ownership stopped being
exclusive. roast found both; review had not.

Dropping the sole consumer used to disconnect the upstream mpsc, because the
consumer *was* the receiver — and that is how `Supply.interval` learned to stop
(`register_interval` retires its deadline-heap entry when `send` fails). With a
template kept alive in the registry the mpsc never hangs up on its own, so the
interval ticked forever and `roast/S17-supply/syntax.t` hung. `send` now reports
`SendError` once the broadcast has had at least one subscriber and every one of
them has been dropped, which is exactly when Raku stops a live Supply's
producer. Before the first subscriber exists the producer keeps sending, so a
`Proc::Async` reader thread that starts before the react loop subscribes does
not lose its first chunk.

A third came from the socket row. Broadcasting made mutsu deliver a socket
connection's bytes to *both* taps whenever the second registered before the
first bytes arrived — invisible when idle, a `make test` failure under load.
Since rakudo hands each chunk to exactly one tap, exclusivity is now an explicit
property a source declares (`SupplyReceiver::mark_exclusive`), and
`take_supply_channel` keeps the old exclusive-transfer behaviour for a source
that declares it. Only the socket connection read Supply does today. "Is this
Supply a broadcast point?" is a property of the source, so every source now
answers it deliberately instead of inheriting an answer from the registry's
implementation.

The second was subtler. Several sites never park their handle in the registry
at all: a `whenever <Promise>` builds a one-shot channel and keeps the receiver
directly, as do the signal watcher and the socket listeners. A template with no
queue of its own silently discarded their events, so every `whenever <Promise>`
stopped firing whenever an interval whenever coexisted with it. The queue is
therefore registered lazily on first poll: an unpolled template accumulates
nothing, and a directly-polled handle registers before it pumps, so the
upstream backlog still reaches it.

## Re-measuring first changed the shape of the problem

The original finding asserted the fault was general to "every channel-backed
Supply", naming `Proc::Async`, the merged `.Supply`, `IO::Socket::Async` read
streams and the scheduled pumps. Measuring all of them against rakudo v2026.07
before designing anything contradicted that in two directions:

- **The socket row was not a divergence at all.** Rakudo does not fan out an
  `IO::Socket::Async` connection's read Supply either — the two taps compete and
  one wins, with the winner varying between runs. mutsu already matched it. The
  finding was reporting agreement as a bug.
- **Five other shapes already worked**: `Supplier.Supply` tapped twice, a late
  tap on a `Supplier`, `Supply.from-list` twice, a `supply { }` block twice (its
  body correctly running once per tap), and `.Channel` on a Supply.

The whole real divergence was the `Proc::Async` output family — `.stdout`,
`.stderr`, the merged `.Supply`, two separate `.stdout` accessor calls, the
derived `.lines`, and `.tap` twice outside `react` — six shapes, all now fixed.

## Live semantics are what ruled out the obvious design

Two of the already-correct rows turned out to constrain the fix more than the
broken ones did. A tap added to a `Proc::Async` output Supply *after* the
process exited gets nothing, and a late tap on a live `Supplier` sees only
values emitted after it tapped — in raku and in mutsu alike. So the naive
fan-out, buffering every value and replaying the buffer to a late tapper, would
have broken behaviour mutsu already had right, turning live supplies into
on-demand ones. Subscribers therefore start empty and are never replayed to.
That also ruled out the "one receiver plus a per-tap cursor over a retained
buffer" alternative, whose log can only be trimmed to the slowest cursor, so one
stalled tap would pin the entire history of a long-running stream.

The cost recorded in the ADR is that an unconsumed tap queues what it does not
read, bounded by the tap being dropped; and that `Tap.close` on one of several
taps of the same channel-backed Supply still stops the source for all of them —
strictly better than the previous behaviour, where the other taps received
nothing at all, but explicitly left for later.

Pinned by `t/supply-fanout-to-multiple-taps.t`, which covers all thirteen
measured shapes including the ones that already passed, and which passes under
real rakudo as well as under mutsu. `supply_channel.rs` also carries unit tests
for the broadcast primitive itself (late subscribers, disconnect after drain,
pruning of dropped subscribers).
