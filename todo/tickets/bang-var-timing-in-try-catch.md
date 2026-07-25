# `$!` is assigned at the wrong time (and with the wrong "no error" value) by `try`/`CATCH`

Found 2026-07-25 while fixing the caller-`$!`-clobbering bug
(`news/2026-07/caller-error-var-survives-a-call.md`). Two divergences remained,
both about *when* and *to what* `try`/`CATCH` writes `$!`. Neither involves a
call, so they are independent of that fix.

## Repro 1 — `$!` is visible too early inside a `CATCH` block

```raku
try {
    die "x";
    CATCH { default { say $!.defined ?? $!.message !! "Nil" } }
}
# raku:  Nil
# mutsu: x
```

raku assigns `$!` only when the `try` **completes**; inside the `CATCH` block
the exception is the topic (`$_`), and the enclosing scope's `$!` has not been
written yet. mutsu writes `$!` before running the handler
(`vm_try_catch_ops.rs:367`, `vm_misc_scope.rs:298`/`352`), so a `CATCH` body
sees the in-flight exception under `$!`.

## Repro 2 — a successful `try` leaves `Nil`, not `Any`

```raku
try { 1 };
say $!.^name;
# raku:  Any
# mutsu: Nil
```

The "no error" value differs. Note that raku's *initial* `$!` (before any
`try`) and a routine's fresh entry `$!` are both `Nil`, so this is specifically
the value a completed-without-error `try` stores
(`vm_try_catch_ops.rs:396`/`426` restore `prior_bang.unwrap_or(Value::NIL)`).

## Why it is not a one-liner

Repro 1 cannot be fixed by simply deferring the write: mutsu's `CATCH` handling
resumes the enclosing block after the handler, and several existing pins read
`$!` in places that would be affected by moving the assignment point. The two
repros also pull in opposite directions — repro 2 wants the no-error value
changed from `Nil` to `Any`, and `Nil`-vs-`Any` identity is its own knot
(PLAN §8.5). Doing both needs a pin covering: `CATCH` body, `try` with and
without an error, nested `try`, a `CATCH` that rethrows, and `$!` after a
routine returns.

## Affected files

- `src/vm/vm_try_catch_ops.rs` (the `$!` writes at 134 / 367 and the
  `prior_bang` restores at 396 / 426)
- `src/vm/vm_misc_scope.rs` (the block-scope `CATCH` twins at 298 / 352)

## Impact

Low on its own — code that reads `$!` inside its own `CATCH` block is unusual
(the topic is the idiomatic access there). Recorded because it is a real
divergence found with a reduced repro, and because both symptoms live in the
same few lines, so whoever touches that code should fix them together.
