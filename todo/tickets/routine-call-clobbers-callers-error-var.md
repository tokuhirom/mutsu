# Any sub/method call resets the caller's `$!` to Nil

`$!` is scoped per routine in raku, so a routine gets a fresh `Nil` on entry —
but the CALLER's `$!` must survive the call. mutsu resets `$!` on entry (which is
right) and then merges the callee's env back over the caller's on return (which
is not), so every call wipes the caller's error variable.

## Repro

```raku
sub f() { 42 }
try { die "boom" };
say $!.^name;      # X::AdHoc      (both)
f();
say $!.^name;      # raku: X::AdHoc   mutsu: Nil
```

The same thing happens through a method call, which makes it easy to hit by
accident:

```raku
class E is Exception { has $.rc; method foo { "f:$.rc" } }
try { E.new(:rc(3)).throw };
say $!.foo;   # f:3    (both)
say $!.rc;    # raku: 3    mutsu: Nil   -- $! was wiped by the .foo call
```

Saving the exception first (`my $saved = $!`) works, so it is purely the
caller-env restore that loses it.

## Root cause

`$!` is set to `Nil` on routine entry (`vm_method_dispatch.rs`'s
`env_mut().insert("!", Value::NIL)`, and the twin in `vm_call_named_inner.rs`).
On return, the frame-restore merge copies every callee env key back into the
restored caller env when the caller already has that key and it is not a callee
local — see the loop in `vm_call_named_inner.rs` (`for (k, v) in self.env().iter()`).
That loop already skips the other per-routine magic names (`_`, `@_`, `%_`,
`__mutsu_callable_id`); `!` belongs in the same skip list but is missing.

## Affected files

Every call path has its own copy of that merge, and they must agree:

- `src/vm/vm_call_named_inner.rs` (two loops: the env merge and the
  `free_var_writes` propagation)
- `src/vm/vm_call_fast.rs`
- `src/vm/vm_call_light.rs`, `src/vm/vm_call_light_typed.rs`
- `src/vm/vm_closure_dispatch.rs` (blocks legitimately SHARE the caller's `$!` —
  a `CATCH` block sets it — so the closure path must NOT get the same skip)

## Why it is not a one-liner

The skip must be added to the routine paths but withheld from the block/closure
paths, and `try`/`CATCH` deliberately writes `$!` in the *enclosing* frame
(`vm_try_catch_ops.rs` saves and restores `prior_bang` by hand). Getting the
routine-vs-block split wrong silently breaks either error propagation out of a
`try` or `$!` visibility inside a `CATCH`, neither of which the current pins
would necessarily catch — the change needs its own pin covering sub, method,
block, `try`/`CATCH` and nested-call combinations.

## Impact

Surfaced 2026-07-25 while fixing PLAN 8.24 (a user `method message` on an
exception): `$!.message` now runs the user method, and the very next `$!.rc`
reads `Nil`. Any code that inspects several fields of `$!` in a row is affected.
