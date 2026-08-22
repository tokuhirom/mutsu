# `[∘]` (function-composition reduce) with an empty operand list doesn't produce a working identity `Callable`

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/operators.rakudoc:1675`).

## Root cause hypothesis

The reduce meta-operator `[OP]` applied to zero operands is documented to return `OP`'s
identity value; for `∘` (function composition), the identity is the identity function, so
`[∘]` alone should evaluate to a `Callable` that returns its argument unchanged:

```raku
my &composed = [∘];
say composed("foo");  # raku: foo
```

mutsu instead produces something that is not a valid, resolvable named sub:

```raku
say composed("foo");
# Unknown function: composed
```

and directly inspecting `[∘]`'s type confirms it isn't a `Callable` at all in mutsu:

```
my $x = [∘]; say $x.WHAT;   # mutsu: (Any)   -- should be some Callable/Block type
```

So `[∘]` on an empty operand list is evaluating to `Any`/`Nil` rather than synthesizing the
identity-function closure, and binding that non-callable to `&composed` leaves the symbol
effectively unusable (hence "Unknown function" when later called by bare name).

## Minimal repro

```raku
my &composed = [∘];
say composed("foo");
```

- `raku`: `foo`
- `mutsu` (`target/debug/mutsu`): `Unknown function: composed` (runtime error)
- Also: `my $x = [∘]; say $x.WHAT;` — `raku`: some Callable type; `mutsu`: `(Any)`

## Affected files (starting point)

- The reduce meta-operator (`[OP]`) implementation for a zero-operand list — likely
  `src/runtime/methods.rs` / wherever `.reduce`/triangle-reduce dispatches per-operator
  identity values (search for how other operators' identity elements are handled for empty
  input, e.g. `[+]` on an empty list returning `0`) and add the `∘` (function composition)
  case, which needs to return an identity *Callable* rather than a scalar identity value.
