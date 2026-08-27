# The merged `Proc::Async` Supply is a live stream, so `whenever $proc` works in `react`

`whenever $proc { ... }` inside a `react` block used to run its body zero times.
The child's output was read, collected, and thrown away:

```raku
my $proc = Proc::Async.new("echo", "test");
react {
    whenever $proc { print "GOT:$_" };
    whenever $proc.start { }
};
print "|pass\n";
```

`raku` prints `GOT:test|pass`; mutsu printed just `|pass`. Writing the coercion
out by hand (`my $s = $proc.Supply; react { whenever $s {...}; ... }`) made no
difference, while the same tap **outside** `react` (`$proc.Supply.tap({...});
await $proc.start`) worked — which localised the fault to how the merged Supply
is produced, not to `whenever`'s source handling.

## Root cause

The merged `.Supply` had no producer at all. `.start()`
(`src/runtime/native_proc_async.rs`) created a `supply_event_channel` for
`stdout_supply_id` and for `stderr_supply_id` and had the two reader threads
push decoded chunks into them, but the merged supply id got nothing. It was
served only *after the fact*, by `replay_proc_taps`
(`src/runtime/methods_collection_ops/socket_inet_proc.rs`), which reads the
`collected_merged` string off the finished `Proc` and calls the registered taps.

`replay_proc_taps` is reached from `replay_settled_proc_taps`, whose only two
callers are the `await` builtin and `Promise.result`. React's drive loop settles
a `whenever <Promise>` subscription through `is_resolved()` / `result_blocking()`
directly and never goes through either — so for a react-driven process the
replay simply never happened. `whenever $proc.stdout { ... }` worked in react
precisely because the per-stream Supply *did* have a channel the drive loop
could take.

## The fix

`.start()` now creates a channel for the merged Supply too, whenever the merge
is actually claimed, and hands a `SupplySender` clone to **both** reader threads
through a new `ChunkSinks` struct: every decoded chunk goes to the reader's own
per-stream Supply and to the merged Supply. The merge is therefore a genuine
live interleave of the two pipes rather than something reconstructed from
`collected_stdout ~ collected_stderr` after the child exits.

Two concurrency points needed care, and both are handled by a recorded fact
rather than by timing:

- **`Done` belongs to neither reader.** The merged stream ends only once *both*
  readers have finished, so its `SupplyEvent::Done` is sent by the `proc-wait`
  thread after it joins them — sending it from a reader would close the merge
  while the other pipe was still producing. An `Arc<AtomicBool>` records that a
  reader already `quit` the merge on an encoding error, so `proc-wait` does not
  then follow a `quit` with a `done`.
- **Delivery must happen exactly once.** A `whenever` on a Supply also registers
  an ordinary tap (`subtest.rs`, "for non-react backward compat"), so once the
  drive loop drains the channel, a later `await`/`.result` on the same `Proc`
  would have replayed the whole output into that same callback a second time.
  `take_supply_channel` now records the supply as live-consumed
  (`mark_supply_live_tapped`) — taking the receiver *is* the transfer of
  ownership, so recording it there makes the channel path and the replay path
  mutually exclusive by construction, whoever the consumer is. The merged
  replay in `replay_proc_taps` consults it, exactly as the per-stream
  `replay_proc_output` already did. When nobody took the merged channel (a plain
  `.tap()` outside react, still served by the replay), the parked receiver is
  discarded instead of pinning a second copy of the whole child output.

## A pre-existing double-delivery fixed on the way

Moving the live-consumed mark into `take_supply_channel` also fixed a real
duplication in the *per-stream* path that predates this work:

```raku
my $proc = Proc::Async.new("echo", "x");
my $s = $proc.stdout;
my $out = '';
my $p = $proc.start;
react { whenever $s { $out ~= $_ }; whenever $p { } };
await $p;
say $out.raku;
```

`raku` says `"x\n"`; mutsu said `"x\nx\n"` — the react drive loop delivered the
chunk and the `await`-time replay delivered it again to the same callback. It
now says `"x\n"`.

## Verification

`t/proc-async-merged-supply-react.t` pins thirteen assertions and passes under
real `raku` as well as under mutsu: the bare `whenever $proc` coercion and the
explicit `.Supply` form; stdout and stderr both reaching the merged tap; a
silent process still ending the Supply and firing `LAST` exactly once; a
16893-byte output arriving whole, line-complete, and in more than one chunk;
`done` inside the merged body ending the react; once-only delivery for a plain
`.tap` + `await`, for a react-driven merged tap followed by `await`, and for the
per-stream case above; and `.Supply` + `.stdout` together still dying with
`X::Proc::Async::SupplyOrStd`.

Nothing in the test pins the interleaving of the two pipes or the chunk
boundaries between them — that is a race between two independent reader threads,
and `raku` does not specify it either. Assertions are on content and on counts
that do not depend on which reader wins.

## Divergences left open

Two `Proc::Async` merge divergences surfaced while building the reference matrix
and are recorded separately, because neither is caused or worsened by this
change:

- `todo/tickets/procasync-merged-tap-after-start-should-throw.md` — tapping the
  merge after `.start()` should throw `X::Proc::Async::TapBeforeSpawn`.
- `todo/deep/supply-channel-has-no-fanout-to-multiple-taps.md` — a
  channel-backed Supply can only be tapped once, so a second `whenever` on the
  same source gets nothing.
