# A `when CX::Warn { .resume }` arm followed by `default` resumes an op-raised warning

A CONTROL block like this one returned `Nil` from its routine as soon as an
opcode raised a warning (`"a" ~ $x` with `$x` undefined):

```raku
CONTROL {
    when CX::Warn { .resume }
    default { return Nil }
}
```

The handler ran and called `.resume`, but execution never continued after the
raise site (#9425). CodeUnit's `CodeUnit.eval` has exactly this shape.

mutsu resumes a warning at its raise site only when the compiler classifies the
CONTROL block as *resume-safe* (`control_block_is_resume_safe`). The classifier
required every arm that could match a `CX::Warn` to end in `.resume`, and it
counted the trailing `default` as one of them. But `when` is first-match: a
warning lands in the first `when CX::Warn` (or `default`) arm, and every arm
after that one is unreachable for it. The classifier now decides on that first
warning-matching arm, skipping arms for other `CX::` types before it.

A handler that still is not resume-safe (`when CX::Warn { .resume if $cond; ... }`)
takes the unwinding path. That path has no resume point for a warning raised by
an op or by a callee, so the handler can only end the block. Fixing that needs
CONTROL handlers to run at the raise site the way ADR-0072 runs CATCH handlers;
it is filed as #9469.

Pin: `t/exceptions/warn-resume-when-arm-before-default.t`.
