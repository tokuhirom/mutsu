# A `Proc::Async` tap is replayed through a composite promise too

mutsu delivers a `Proc::Async` output tap once, later, on the main thread:
`native_supply_mut_methods.rs` deliberately does not start a live-channel
consumer for a proc output supply, and `replay_proc_taps` flushes the collected
output when the process's promise is awaited. The hook fired on an `await` /
`.result` whose *result* was a `Proc` — which is only true of the promise
`$proc.start` returns.

That made the standard "run it, but give up after N seconds" idiom deliver
nothing, because a `Promise.anyof`/`allof` composite resolves to a plain `True`:

```raku
my $p = Proc::Async.new: $*EXECUTABLE.absolute, '-e', 'say "B"';
my $s = '';
$p.stdout.tap: -> $a { $s ~= $a };
my $pr = $p.start;
await Promise.anyof: Promise.in(10), $pr;
say $s.raku;        # raku: "B\n"   mutsu: ""
await $pr; say $s.raku; # mutsu: "B\n" — the tap fired only here
```

`Promise.allof` already recorded its source promises in a registry (the react
driver waits on them to settle a `whenever Promise.allof(...)`). That registry
now records `anyof`'s sources as well, tagged with which combinator produced
them, and `await`/`.result` walk it: for a promise whose own result is not a
`Proc`, every source that has *already settled* gets its taps replayed, and the
walk recurses so a composite of composites works. A still-`Planned` source is
skipped deliberately — waiting on it would reintroduce the hang the composite
exists to avoid. The kind tag keeps the react driver's behaviour byte-identical:
it blocks on the full source list only for `allof`.

This is the shallow fix. The real one is push-delivery for proc output supplies,
the way ADR-0008 made the other supplies push-based (#4636); the "replay at
await" design is what makes `.tap` observably different from `react`/`whenever`
on the same stream. But it closes the second of the two blockers in
`todo/tickets/retire-native-test-util-overrides.md`: `Test::Util`'s
`doesn't-hang` collects the child's output in a `.stdout.tap` closure and then
does exactly the `await Promise.anyof: Promise.in($wait), $prog.start` above.

Pin: `t/composite-promise-replays-proc-taps.t` (anyof, allof, `.result` on the
composite, and a `whenever Promise.allof` regression guard). Note the filename
avoids a `proc-async-*` prefix — `.gitignore` claims that pattern for the
scratch files `S17-procasync` tests leave behind.
