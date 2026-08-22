# `.resume` on a caught exception does not resume execution at the `die`'s call site inside a nested sub

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/exceptions.rakudoc:428`).

## Repro

```raku
sub bad-sub {
    die "Something bad happened";
    return "not returning";
}

{
    my $return = bad-sub;
    say "Returned $return";
    CATCH {
        default {
            say "Error ", .^name, ': ', .Str;
            $return = '0';
            .resume;
        }
    }
}
```

- `raku`:
  ```
  Error X::AdHoc: Something bad happened
  Returned not returning
  ```
- `mutsu` (`target/debug/mutsu`):
  ```
  Error X::AdHoc: Something bad happened
  ```
  (execution stops after the `CATCH` block runs — `bad-sub` never resumes past its
  `die`, `return "not returning"` never executes, `bad-sub`'s call never completes, and
  the enclosing block's `say "Returned $return"` is never reached.)

## Root cause hypothesis

Real Raku's `.resume` on an exception is a genuine continuation: calling it inside a
`CATCH`/`default` handler causes execution to jump back to the exact statement *after*
the `die` call, inside `bad-sub`'s own call frame, and `bad-sub` then completes normally
(reaching its `return "not returning"`), unwinding back to the original call site
(`my $return = bad-sub`) as if the `die` had simply been a no-op statement.

This requires the VM to preserve enough state at the `die`/throw site — the exact
program-counter position and live locals of every frame between the `die` and the
handler — to be able to re-enter execution there after the handler decides to resume,
rather than just discarding the unwound frames the way a normal exception propagation
does. mutsu's existing `.resume` support (see `try_resume_safe_control_inline` and the
`CONTROL`/`warn` resume work referenced in `todo/deep/vendor-real-test-module.md`)
appears to be built for **CONTROL-flow exceptions** (`warn`, which is implicitly
resumable and whose resume point is typically shallow/simple), not for an arbitrary
`die`-based exception resumed from several frames up. Whether the same mechanism can be
generalized, or whether `die`+`.resume` needs its own frame-preserving throw path, is
an open design question.

## Why this is deep

Implementing this correctly means the VM's exception-unwinding machinery must be able
to *not* unwind (or must be able to reconstruct enough continuation state to resume) an
arbitrary call-frame chain between an arbitrary `die` site and its handler — this is
architecturally a form of resumable/continuation-based exception handling, not a
narrow bug fix. It likely interacts with:

- How mutsu currently unwinds Rust call frames on a `die` (native `Result`/panic-style
  propagation vs. preserved VM-level frames).
- The existing `CONTROL`/`warn` resume machinery (`try_resume_safe_control_inline`,
  `src/vm/` resume-safe control handling) — worth investigating whether that mechanism
  already preserves what's needed and just isn't wired up for regular `X::*`
  exceptions, or whether it fundamentally can't reach nested-sub call depth.

## Affected files (starting point)

- `src/vm/vm_control_ops.rs` (try/CATCH/`.resume` handling).
- Exception-throw / call-frame-unwind machinery generally — grep for where `die`
  triggers Rust-level unwinding vs. VM-level frame teardown.
- `todo/deep/vendor-real-test-module.md`'s `warn-resumes-at-the-raise-site.t` sections
  for prior art on the existing (CONTROL-exception-only) resume mechanism.
