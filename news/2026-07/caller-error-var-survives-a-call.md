# The caller's `$!` now survives a sub or method call

`$!` is scoped per routine in raku: a sub or method gets a fresh `Nil` on entry,
but the CALLER's `$!` must survive the call. mutsu reset `$!` on entry (which is
right) and then merged the callee's env back over the caller's on return (which
is not), so every call wiped the caller's error variable.

```raku
sub f() { 42 }
try { die "boom" };
say $!.^name;      # X::AdHoc      (both)
f();
say $!.^name;      # raku: X::AdHoc   mutsu was: Nil
```

The easiest way to hit it was to read two fields of an exception in a row, since
the first read is itself a method call:

```raku
class E is Exception { has $.rc; method message { "m:$.rc" } }
try { E.new(:rc(3)).throw };
say $!.message;   # m:3   (both)
say $!.rc;        # raku: 3   mutsu was: Nil -- $! was wiped by the .message call
```

It surfaced while landing the user-`method message` support (PLAN 8.24), where
`$!.message` started running the user method and the very next `$!.rc` read
`Nil`. Saving the exception first (`my $saved = $!`) worked, which pointed at
the caller-env restore rather than at the exception value.

## Root cause

`$!` is set to `Nil` on routine entry. On return, the frame-restore merge copies
every callee env key back into the restored caller env when the caller already
has that key and it is not a callee local. That loop already skips the other
per-routine magic names (`_`, `@_`, `%_`, `__mutsu_callable_id`); `!` belonged in
the same skip list and was missing. A second symptom fell out of the same hole:
a callee that ran its own `try { die … }` exported *its* `$!` to the caller.

## Fix

Every routine-call path got the skip, via a shared
`runtime::utils::is_routine_scoped_error_var` so the copies cannot drift apart:
`vm_call_named_inner.rs` (the env merge and the `free_var_writes` propagation),
`vm_call_fast.rs` (both the scoped-overlay and the clone-merge branches),
`vm_call_light.rs` and `vm_call_light_typed.rs` (each has a swap-path merge and a
reused-frame `retain_overlay`), and `merge_method_env` in `vm_method_dispatch.rs`.

The block/closure path deliberately does **not** get it: a bare block shares its
enclosing routine's `$!`, and a `CATCH` block *writes* it there, so skipping the
merge for blocks would break error propagation out of a `try`. On the compiled
paths the distinction is exactly `cf.code.is_routine`, so the skip is gated on
that rather than applied to the whole merge.

Pin: `t/caller-error-var-survives-call.t` — plain sub, method, multi, proto,
private method, `submethod BUILD`, recursion, `.&func`, the two-field-read shape,
a callee with its own `try`, a routine's fresh entry `$!`, a bare block sharing
the enclosing `$!`, a caught callee error that *does* set the caller's `$!`, and
a successful `try` clearing it. Verified identical under `raku`.

Two neighbouring divergences found while writing that pin are left open in
`todo/tickets/bang-var-timing-in-try-catch.md`: mutsu makes `$!` visible inside a
`CATCH` body (raku assigns it only when the `try` completes), and a successful
`try` leaves `Nil` where raku leaves `Any`. Both are about `try`/`CATCH`'s own
write timing, not about calls.
