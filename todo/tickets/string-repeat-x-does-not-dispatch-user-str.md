# Infix `x` (string repetition) renders its left operand with `.gist` instead of dispatching a user `Str`

Found while fixing
[`reduce-metaop-numeric-coercion-bypassed`](../../news/2026-08/reduce-metaop-numeric-coercion-bypassed.md).
That ticket's root cause was "a pure two-`Value` function cannot dispatch a user
method, so it falls back to `.gist`", and it was fixed for the reduce
meta-operator on both the numeric and the string side. The **binary `x`
operator** has the identical bug and is a separate code path, so it was left
out of that fix.

## Minimal repro

```raku
class S { has $.s; method Str { $!s } }
my $a = S.new(s => "ab");
say $a ~ $a;   # abab   -- infix `~` dispatches the user Str (correct)
say $a x 2;    # raku: abab
               # mutsu: S()S()
```

`~` is correct because `exec_concat_op` runs both operands through
`coerce_stringy_operand` before handing them to the pure `concat_values`.
Infix `x` does not.

## Suggested fix

Whatever opcode implements infix `x` should run its **left** operand through
`Interpreter::coerce_stringy_operand` first, mirroring `exec_concat_op`
(including the `reconcile_caller_after_internal_dispatch` drain, since a user
`Str` method can mutate a captured-outer caller lexical). Only the left operand:
the right one is a repetition count and must stay numeric, so it must NOT be
stringified.

Worth checking the neighbours in the same pass — `xx` (list repetition) and the
`X`/`Z` cross/zip meta-op forms that build strings — for the same omission.

## Affected files (starting point)

- `src/vm/vm_arith_ops.rs` (repetition ops) and/or
  `src/runtime/builtins_operators_repeat.rs`
- `src/vm/vm_coerce_concat_ops.rs` — `coerce_stringy_operand`, the helper to
  reuse, and `exec_concat_op`, the shape to copy.

## Pin

`t/numeric-coercion-gaps.t` already pins the working `~` / `[~]` / `[lt]` cases
next to where an `x` assertion would go.
