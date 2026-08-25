# `whenever $proc { ... }` never runs its body inside a `react` block

Found while fixing `todo/tickets/procasync-untapped-stdout-not-passthrough.md`
(now `news/2026-08/`). It is **pre-existing and independent** of that fix — it
reproduces identically with or without it.

## Minimal repro

```raku
my $proc = Proc::Async.new("echo", "test");
react {
    whenever $proc { print "GOT:$_" };
    whenever $proc.start { }
};
print "|pass\n";
```

- `raku` (2026.06): `GOT:test|pass`
- `mutsu`: `|pass` — the body never runs; the merged output is collected and
  thrown away.

Writing the coercion out by hand makes no difference, which localises the bug to
the react drive loop rather than to `whenever`'s source handling:

```raku
my $proc = Proc::Async.new("echo", "test");
my $s = $proc.Supply;
react { whenever $s { print "GOT:$_" }; whenever $proc.start { } };
```

- `raku`: `GOT:test|pass`
- `mutsu`: `|pass`

The same thing **outside** `react` works correctly, which is the useful contrast:

```raku
my $proc = Proc::Async.new("echo", "test");
$proc.Supply.tap({ print "GOT:$_" });
await $proc.start;          # prints GOT:test
```

## Root cause

The merged `.Supply` has no channel feeding it. `.start()`
(`src/runtime/native_proc_async.rs`) creates a `supply_event_channel` for
`stdout_supply_id` and `stderr_supply_id` and has the two reader threads push
chunks into them, but the merged supply id gets **nothing** — it is only ever
served after the fact, by `replay_proc_taps`
(`src/runtime/methods_collection_ops/socket_inet_proc.rs`), which reads the
`collected_merged` string off the resulting `Proc` and calls the merged taps.

`replay_proc_taps` is reached from `replay_settled_proc_taps`, whose only two
callers are the `await` builtin (`src/runtime/builtins_system_async.rs`) and the
`Promise.result` method (`src/runtime/methods_promise.rs`). React's drive loop
(`src/vm/vm_react_subscriptions.rs`) settles a `whenever <Promise>` subscription
through `promise.is_resolved()` / `result_blocking()` directly and never goes
through either, so the replay simply never happens for a `react`-driven proc.

`whenever $proc.stdout { ... }` works in react precisely because the per-stream
supply *does* have a channel, which the drive loop can poll like any other
channel-backed supply.

## Why it is a ticket and not a one-liner

The obvious fix — give the merged supply its own channel and have both reader
threads clone a `SupplySender` into it — is small in isolation (`SupplySender`
is already `Clone`), but it needs care on two concurrency points:

- `SupplyEvent::Done` for the merged stream may only be sent after **both**
  reader threads have finished, so it belongs in the `proc-wait` thread after the
  joins, not in either reader.
- `replay_proc_taps` must not then double-deliver the merged output to a tap the
  channel already served. The per-stream path solves exactly this with
  `mark_supply_live_tapped` / `is_supply_live_tapped`; the merged path currently
  guards only with `mark_supply_replayed`, so it needs the equivalent treatment.

Both are on the Proc::Async concurrency path, which has an open crash-class
finding of its own (`todo/deep/procasync-stress-segv.md`), so the change wants
its own PR and its own targeted `roast/S17-procasync` run rather than riding
along with an unrelated fix.

## Affected files

- `src/runtime/native_proc_async.rs` — `.start()`'s channel creation and the two
  reader threads; the `proc-wait` thread's join point.
- `src/runtime/methods_collection_ops/socket_inet_proc.rs` — `replay_proc_taps`'s
  merged-supply replay and its once-only guard.
- `src/vm/vm_react_subscriptions.rs` — where a promise-source subscription
  settles, if the alternative (replaying there) is preferred instead.
