# `return-rw` doesn't return a mutable container

Discovered via the doc-diff harness on `raku-doc/doc/Language/control.rakudoc` (around line
1375).

## Repro

```
sub s() { my $a = 41; return-rw $a }
say ++s();
```

- raku: `42` (`return-rw` returns the actual container `$a` is bound to, so `++` can mutate it
  through the call result)
- mutsu: `Cannot resolve caller prefix:<++>(...); the parameter requires mutable arguments`

## Root cause guess

`return-rw` is either unimplemented and silently falling back to plain `return`'s by-value
semantics, or implemented but not preserving the rw/container-binding flag on the returned
value all the way through the call-return path, so downstream mutation (`++`) sees a
non-mutable value.

## Affected files (starting point)

- `src/runtime/calls.rs` / `src/vm/vm_call_ops.rs` — return-value handling, `rw` propagation
- Grep for `"return-rw"` / `ReturnRw` in the parser/compiler/VM to find the current (likely
  stubbed) implementation

## Suggested next step

Check whether `return-rw` exists at all as a distinct AST/opcode form vs. just being parsed as
`return`; if it's missing, this needs the same rw-container plumbing that `is rw` return-type
traits and `sub f() is rw { $a }` already use elsewhere in the codebase.
