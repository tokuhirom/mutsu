# `.resume` now returns to the `die`'s own call site inside a nested sub

`todo/deep/resume-does-not-return-to-die-call-site-in-nested-sub.md` reported that
`.resume` on a caught exception never came back to the statement after the `die`
when the `die` was inside a called sub:

```raku
sub bad-sub { die "Something bad happened"; return "not returning" }
{
    my $return = bad-sub;
    say "Returned $return";
    CATCH { default { say "Error ", .^name; $return = '0'; .resume } }
}
```

raku prints the error line and then `Returned not returning`; mutsu printed the
error line and stopped. That is fixed — the whole family is, from one sub-frame of
depth to three, through loops, nested blocks, `try {}`, and `.throw` of a user
`Exception` subclass. The decision is recorded as
[ADR-0072](../../docs/adr/0072-a-resumable-exception-runs-its-handler-at-the-throw-point.md);
the measured before/after table lives there and every row of it is pinned by
`t/exception-resume-cross-frame.t`.

## The ticket's diagnosis was two-thirds wrong

The ticket said the existing machinery was "built for CONTROL-flow exceptions
(`warn` ...), not for an arbitrary `die`", and that `warn`'s resume point was
"typically shallow/simple". Re-measuring first (the standing rule) killed both
claims:

- `die` + `.resume` already worked when the `CATCH` was in the same block **and**
  when it was in the same *sub body*.
- `warn` + `.resume` already worked **across frames** — two sub-frames of depth
  resumed correctly.

The one axis that failed was an exception raised in a frame *below* the one that
installed the handler. `resume_ip` is a `(code_fingerprint, ip)` pair and
`take_resume_ip_for` deliberately discards a point recorded in another code
object, because mutsu's VM recurses on the **Rust** stack: by the time the `Err`
reaches the owning region, every frame between the `die` and the handler has been
popped by `?`. There is no continuation left to resume into, so no amount of
bookkeeping on the unwinding path could have fixed it.

## Two measurements decided the design

Widening the probe past the ticket's repro is what produced the answer:

- A handler that resumes reads a `my $*WHERE` set **inside the dying sub**. So
  rakudo runs the handler in the dynamic scope of the throw.
- The handler runs **before** the dying sub's `LEAVE` phaser.

Together those say rakudo does not save a continuation either: it runs the
`CATCH` handler at the throw point, before unwinding, and `.resume` is just the
handler returning while the `die` evaluates to `Any`. mutsu already had exactly
that shape for CONTROL — `try_resume_safe_control_inline` runs a resume-safe
`CONTROL` block inline at the `warn` raise site — which is why cross-frame `warn`
worked all along. ADR-0072 generalizes that one mechanism to `CATCH` instead of
inventing a second.

## What shipped

`OpCode::TryCatch` gained `catch_resume_capable`, set by scanning the emitted
CATCH op range for a `.resume` method call; every `try` and every block with a
`CATCH` registers a `CatchHandlerEntry` for the duration of its protected body,
and only a resume-capable one carries the bytecode to run inline. A `die`
(`OpCode::Die` for the statement form, `builtin_die` for the expression form) and
an `Exception.throw` consult the innermost entry: if it can resume, the handler
runs there and then, against the installing frame's lexicals reconstructed from
`env` (the helper is now shared with the CONTROL path). A handler that runs inline
but does *not* resume still has to abandon its block, so the throw site tags the
error with the region's token and verdict, and the region applies it while
unwinding rather than running the handler twice.

Two guards keep the change strictly additive. A throw raised while the installing
region's own code object is executing is left on the pre-existing frame-local
path — the inline path would swap `self.locals` for an env reconstruction of the
same frame and drop the handler's writes to the live slots (caught by `t/resume.t`
and `t/topic-quoted-method-call.t` during development). And a region that cannot
resume still registers a blocking marker, so a deep throw never skips a nearer
handler in favour of a resuming one further out.

## What is left

Two rows of the table are still open, both recorded in ADR-0072 and marked `todo`
in the pinning test:

- Resuming an exception that an inner, non-resume-capable `CATCH` **rethrew**
  needs the whole handler chain run inline, innermost first (Slice 2).
- For a `CATCH` that cannot resume, the handler still runs *after* unwinding, so
  it does not see the throw's dynamic scope and runs after the dying frame's
  `LEAVE` phaser. Closing that means ungating the inline path for every `CATCH`
  (Slice 3), which is what rakudo does but is a much wider blast radius than one
  PR should take on its own.
