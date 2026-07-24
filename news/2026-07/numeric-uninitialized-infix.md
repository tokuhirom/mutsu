# Numeric comparison of a bare numeric type object throws X::Numeric::Uninitialized

A bare concrete-numeric type object (`Int`, `Num`, `Rat`, `FatRat`, `Real`,
`Bool`) used as an operand of a numeric comparison operator
(`== != < > <= >= <=>`) now throws `X::Numeric::Uninitialized`, matching Rakudo.

Previously the equality/three-way ops (`==`, `!=`, `<=>`) silently coerced the
type object to `0`, so `Int == 0` wrongly returned `True`; and the relational
ops (`< <= > >=`) threw a bare `X::AdHoc` carrying the right message but the
wrong exception type.

The error is fatal — it is **not** suppressed by `quietly` — and carries the
standard message (`Use of uninitialized value of type Int in numeric context`)
plus a `.type` attribute holding the offending type object.

## Implementation

- `RuntimeError::numeric_uninitialized(type_name)` (`src/value/error_construct.rs`)
  builds the typed `X::Numeric::Uninitialized` exception with `message` and
  `type` attributes.
- `check_type_object_in_numeric_context` (`src/vm/vm_comparison_ops.rs`) — the
  predicate the relational ops already used — now throws this typed exception
  instead of a bare `X::AdHoc`, and its type set was corrected to the concrete
  numeric types that Rakudo hard-errors on (`Int Num Rat FatRat Real Bool`).
  `Complex` and native `int`/`num` were removed: Rakudo warns-and-coerces for
  those rather than throwing.
- The equality ops (`==`, `!=`), the native `!=`, and the three-way `<=>` gained
  the check at their call sites. Generic `cmp`/`before`/`after` deliberately do
  not throw (they compare the type object as-is, matching Rakudo).

## Not covered (deferred — see PLAN.md)

The arithmetic operators (`+ - * / % **`) should also throw
`X::Numeric::Uninitialized` on a bare numeric type object (`Int + 1`), but they
were left unchanged: mutsu desugars the assignment metaop `$a += 0.1` to the
same `Add` opcode as bare infix, and that form must keep working. Rakudo's
`METAOP_ASSIGN` uses the operator's identity element (0 for `+`, 1 for `*`) when
the container holds a type object, so `my Rat $a; $a += 0.1` yields `0.1`
without throwing (roast `S32-num/rat.t`). Making the bare-infix arithmetic case
throw requires a compiler-level distinction between the two forms plus
`METAOP_ASSIGN` identity semantics.
