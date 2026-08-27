# A channel-backed Supply can only be tapped once — a second `whenever` gets nothing

Every channel-backed Supply in mutsu (`Proc::Async` stdout/stderr, the merged
`.Supply`, `IO::Socket::Async` read streams, the scheduled pumps) is represented
by a single `mpsc` receiver parked in the global `supply_channel_map` under the
supply's id. `take_supply_channel` **removes** it, so whichever consumer asks
first owns the whole stream and every later consumer of the same Supply finds
nothing at all.

Raku Supplies fan out: tapping one twice gives both taps every value.

## Repro

```raku
my $proc = Proc::Async.new("echo", "two");
my $s = $proc.stdout;
my ($x, $y) = ('', '');
react { whenever $s { $x ~= $_ }; whenever $s { $y ~= $_ }; whenever $proc.start { } };
say "x=", $x.raku, " y=", $y.raku;
```

- `raku` (2026.06): `x="two\n" y="two\n"`
- mutsu: `x="two\n" y=""`

The merged Supply behaves the same way (`whenever $proc` twice in one react
block), and so does a socket read stream. It is not specific to `Proc::Async`.

## Why it is deep

The single-receiver design is load-bearing in a lot of places. `take_supply_channel`
has ~10 call sites (the react drive loop in `vm_react_loop.rs` and
`react_died.rs`, the `.start()` live act-loop pump, `zip`, `.list`/`.Array`
materialisation, the `Proc::Async` stdin feeder, `supply_promise.rs`), and
several of them rely on "taking it" being an exclusive transfer of ownership —
including the `Proc::Async` replay guard, which uses exactly that fact to decide
the await-time replay must stand down
(`news/2026-08/procasync-merged-supply-is-live-in-react.md`).

A real fix means giving these Supplies a fan-out point: one owner thread (or the
drive loop) drains the underlying channel and re-broadcasts to N registered
consumers, the way the *supplier* registry (`state_supplier.rs`) already does for
`Supplier`-backed Supplies. That is plausibly the right unification — a
channel-backed Supply becomes a supplier fed by a channel — but it changes the
identity and lifetime of every channel-backed source, so it wants a design pass
rather than a patch. Note also that Raku distinguishes `live` from `on-demand`
supplies here: a second tap on a live Supply sees only values emitted *after* it
taps, so a naive "buffer everything and replay to late tappers" fan-out would be
wrong in the other direction.

Affected files: `src/runtime/native_methods/state.rs`
(`supply_channel_map` / `take_supply_channel` / `has_supply_channel`),
`src/vm/vm_react_loop.rs`, `src/runtime/react_died.rs`,
`src/runtime/native_supply_mut_methods.rs`, `src/runtime/native_proc_async.rs`.

Found while fixing `news/2026-08/procasync-merged-supply-is-live-in-react.md`;
pre-existing and independent of it (it reproduces identically on `.stdout`,
which has had a channel all along).
