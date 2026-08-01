# An `is rw` argument writeback through a closure loses updates after the first call

Minimal repro:

```raku
my $i = 0;
sub g($o is rw) { $o++ }
my $th = { g($i) };
$th(); $th();
say $i;   # raku: 2 — mutsu: 1
```

The first invocation writes back correctly (`$i` becomes 1); the second
invocation reads the closure's CREATION-TIME captured value (0) again,
increments it, and writes 1 back. Direct mutation through the captured cell
works (`my $th = { $i++ }; $th(); $th()` yields 2), so the shared-cell capture
itself is fine — it is the `is rw` writeback path (`apply_pending_rw_writeback`,
`src/vm/vm_env_helpers.rs`) that resolves the source by name into the closure
frame's env and writes a plain value there instead of writing THROUGH the
captured cell, desynchronizing the closure's captured copy from the outer
variable.

Affected files: `src/vm/vm_env_helpers.rs` (`apply_pending_rw_writeback`),
closure capture in `src/vm/vm_closure_dispatch.rs`.

Why it is large: this sits in the env/locals dual-store area
(`PLAN.md` §6 Slice F, `project-slice-f-reverse-sync-campaign`) — the same
writeback-identity problem the reverse-sync campaign is paying down, and a
blind point fix in `sync_env_from_locals`-adjacent code is exactly what the
campaign notes warn against.

Workaround in place: `EXPR xx N` with a small literal N unrolls inline in the
current frame instead of building a thunk closure (compiler/expr_binary.rs),
which is what HTTP::HPACK's `decode-str($packed, $idx) xx 2` needs. The
closure path (dynamic `xx $n` with an rw-arg lhs, or any explicit closure
calling an rw-arg sub on a captured lexical more than once) still loses
updates.
