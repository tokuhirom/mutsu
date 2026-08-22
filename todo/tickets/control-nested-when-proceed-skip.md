# `proceed` inside a bare block used with a trailing `when` statement modifier doesn't skip the enclosing `when`

Discovered via the doc-diff harness on `raku-doc/doc/Language/control.rakudoc` (around line
973).

## Repro

```
given 42 {
    when * > 41 {
        { "A".say; proceed } when * > 41;
        "B".say;
    }
}
```

- raku: prints `A` only — `proceed` unwinds out of the enclosing `when * > 41 { ... }` block
  entirely, so `"B".say` is never reached
- mutsu: prints `A` then `B` — the `proceed` only exits the inner bare block/statement-modifier
  `when`, not the enclosing `when` block

## Root cause guess

`proceed`'s control-flow target is presumably resolved to "the nearest enclosing `when`/
`given`", but when the nearest syntactic `when` is a *statement-modifier* form
(`{ ... } when COND;`) wrapping a bare block, mutsu treats that inner form as the unwind target
instead of continuing the search outward to the real enclosing `when` block.

## Affected files (starting point)

- `src/vm/vm_control_ops.rs` — given/when/`proceed`/`succeed` handling

## Suggested next step

Compare how mutsu tracks the "current when/given frame" stack for `proceed` to unwind through,
and check whether a statement-modifier `when` pushes a frame that it shouldn't (or fails to
delegate to the outer frame when its own block completes).
