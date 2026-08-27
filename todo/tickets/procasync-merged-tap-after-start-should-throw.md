# Tapping a `Proc::Async` merge after `.start()` should throw `X::Proc::Async::TapBeforeSpawn`

Rakudo refuses to tap the merged output Supply of an already-started
`Proc::Async`, because the child is already writing and the merge would silently
drop whatever it produced before the tap arrived. mutsu accepts it and delivers
nothing.

## Repro

```raku
my $proc = Proc::Async.new("echo", "afterstart");
my $got = '';
my $p = $proc.start;
my $err = '';
try {
    react { whenever $proc { $got ~= $_ }; whenever $p { } };
    CATCH { default { $err = .^name ~ ': ' ~ .message } }
}
say "got=[$got] err=[$err]";
```

- `raku` (2026.06):
  `got=[] err=[X::Proc::Async::TapBeforeSpawn+{X::React::Died}: To avoid data races, you must tap merge before running the process]`
  (the child's `afterstart` also lands on the parent's own stdout, since the
  stream was never claimed).
- mutsu: `got=[] err=[]` — the same empty result, but silently.

The output being empty is already correct for both: at `.start()` time the merge
is unclaimed, so stdout is inherited rather than piped. What is missing is the
diagnosis.

## Why it is not a one-liner

The per-stream accessors already do exactly this check — `native_proc_async.rs`'s
`"stdout" | "stderr"` arm throws `X::Proc::Async::TapBeforeSpawn` when
`attrs<started>` is true. The merged equivalent cannot just be copied into the
`"Supply"` arm, because the shape that matters (`whenever $proc { ... }`) never
calls `.Supply` at all: it reaches the merged Supply by coercion. Rakudo's check
is at **tap** time, not accessor time — `my $s = $proc.Supply` before `.start`
followed by a tap after it is rejected too, whereas a `.stdout` Supply fetched
early and tapped late is fine. So the check belongs wherever a tap/`whenever`
registration lands on a Supply that is a `Proc::Async` merge, and needs the
owning proc's `started` flag reachable from there — which the Supply value does
not currently carry.

Affected files: `src/runtime/native_proc_async.rs` (the `"Supply"` arm and the
merged supply id's attributes), `src/runtime/subtest.rs` (the `whenever`
registration path), `src/runtime/native_supply_mut_methods.rs` (`.tap`).

Found while fixing `news/2026-08/procasync-merged-supply-is-live-in-react.md`;
independent of it and unchanged by it.
