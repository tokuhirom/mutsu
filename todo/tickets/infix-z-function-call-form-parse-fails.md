# `infix:<Z>(...)` function-call form fails ("Two terms in a row"), while other `infix:<...>` operators work

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/operators.rakudoc:3177`).

## Root cause hypothesis

Calling an operator by its fully-qualified function name (`infix:<OP>(lhs, rhs)`) is a general
Raku mechanism and works for ordinary operators in mutsu:

```raku
say infix:<+>(1,2);          # 3            -- OK
say infix:<X>(<a b>,<c d>);  # ((a c)...)   -- OK
say infix:<Z>(<a b>,<c d>);  # BROKEN
```

Specifically `infix:<Z>` fails — not at parse time (`--dump-ast` succeeds and shows a normal
`Call { name: Symbol("infix:<Z>"), args: [...] }` node), but at **runtime**, where it raises a
`RuntimeError` whose message text is literally `"Two terms in a row"` (the same string the
*parser* uses for a completely different situation — an unexpected second term). That reused
error string, appearing from a runtime call-dispatch path rather than the parser, suggests
`infix:<Z>` dispatch takes a code path that re-invokes some parsing/matching logic keyed on the
literal name `"Z"` (colliding with the `Z`/zip meta-op or sequence-operator special-casing
elsewhere in the interpreter) instead of just dispatching to the same native `Z` binary-op
implementation that `<a b> Z <c d>` (infix syntax) already uses correctly.

`&infix:<Z>` (as a bare term, no call) also fails when *called* via `&infix:<Z>(...)`, so the
gap is not specific to the bareword-call form — it's in the callable itself / how it's looked
up and invoked.

## Minimal repro

```raku
say infix:<Z>(<a b>,<c d>);
```

- `raku`: `((a c) (b d))`
- `mutsu` (`target/debug/mutsu`): dies at runtime:
  ```
  Two terms in a row
    in block <unit> at -e line 1
  ```
- Compare: `say infix:<X>(<a b>,<c d>);` works correctly in mutsu (`((a c) (a d) (b c) (b d))`),
  ruling out a generic "infix:<...>() call form" gap — this is `Z`-specific.

## Affected files (starting point)

- Wherever `Call { name: Symbol("infix:<Z>") }` gets dispatched at runtime (likely
  `runtime/calls.rs` / `runtime/dispatch.rs`, or a builtin lookup table keyed by operator name)
  — search for how the bare string `"Z"` is special-cased (`src/vm/vm_meta_ops.rs`,
  `src/vm/vm_dispatch_helpers.rs`, `src/vm/vm_misc_reduction_exec.rs` all reference `"Z"`
  specially and are good starting points).
