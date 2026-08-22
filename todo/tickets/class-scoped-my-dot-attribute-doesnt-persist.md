# `my $.counter` (class-scoped shared "attribute") doesn't persist mutations across method calls

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/traits.rakudoc:42`).

## Root cause hypothesis

`my $.counter;` inside a class body declares a lexical scalar with the `.` twigil — a class-level
(shared-across-all-instances, and across calls on the type object itself) counter accessible as
`$.counter` from methods, distinct from a `has $.counter` per-instance attribute. mutsu appears to
either re-initialize this variable on every method call, or not write back the post-increment's
mutation to the shared slot at all — each call sees the same starting value.

## Minimal repro

```raku
class Foo {
    my $.counter;
    method imm() {
        return $.counter++;
    }
}
say Foo.imm for ^5;
```
- `raku`: `0 1 2 3 4` (five lines)
- `mutsu`: `0 0 0 0 0` (five lines) — the counter never advances

The original doc example additionally applies `is repr('Uninstantiable')` and indexes into a
constant array with the counter (`@IMM[ $.counter++ mod @IMM.elems ]`), producing the same
"always the first element" symptom (`Innie` repeated 10 times instead of cycling through
`Innie Minnie Moe`), but neither of those extra pieces is needed to reproduce the bug — the bare
`my $.counter; method imm() { return $.counter++ }` case above already shows it.

## Affected files (starting point)

- Class-body `my $.name` declaration handling (as opposed to `has $.name`) — search for how a
  `my`-declared, dot-twigil'd class variable is registered and how `$.counter` reads/writes route
  to it from inside a method body. Likely in `src/runtime/class.rs` or the compiler's
  attribute/class-var resolution (`compiler/expr.rs` — dot-twigil variable compilation) and
  wherever class-level (vs. instance-level) storage for such variables lives.
