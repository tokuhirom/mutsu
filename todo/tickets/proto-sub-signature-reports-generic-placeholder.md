# `.signature` on a `proto` sub reports a generic `($arg0)` placeholder instead of the declared signature

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/haskell-to-p6.rakudoc:263`).

## Root cause

`Interpreter::callable_signature` (`src/runtime/accessors_state.rs`) computes a callable's
displayed signature. For a `ValueView::Routine { name, .. }` (the `&name` form `.signature`
is typically called on), it looks up the routine via `self.resolve_function(&name.resolve())`
— but `resolve_function` (`src/runtime/resolution.rs`) only consults
`self.registry().functions`, never `self.registry().proto_functions`. A `proto` sub with
no other multi candidate registered under `functions` is therefore never found, and
`callable_signature` falls through to the generic placeholder:

```rust
(vec!["arg0".to_string()], Vec::new())
```

A plain (non-`proto`) `sub` with the identical signature shape works correctly — the bug
is specific to `proto` routines.

## Minimal repro

```raku
proto greeting (Str \name --> Str) {*}
say &greeting.signature;
```

- `raku`: `(Str \name --> Str)`.
- `mutsu` (`target/debug/mutsu`): `($arg0)`.

Confirmed the sigilless-parameter (`\name`) shape isn't the trigger — a plain named
parameter reproduces identically:

```raku
proto greeting (Str $name --> Str) {*}
say &greeting.signature;   # mutsu: ($arg0)  (raku: (Str $name --> Str))
```

And confirmed a non-`proto` sub with the same signature is unaffected:

```raku
sub greeting (Str \name --> Str) { name }
say &greeting.signature;   # mutsu: (Str \name --> Str)  -- correct
```

## Affected files (starting point)

- `src/runtime/accessors_state.rs` (`callable_signature`, the `ValueView::Routine` arm)
- `src/runtime/resolution.rs` (`resolve_function` — needs a `proto_functions` fallback, or
  `callable_signature` needs to consult `registry().proto_functions` directly when
  `resolve_function` returns `None`)
