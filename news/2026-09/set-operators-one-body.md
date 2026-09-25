# The six set operators share one body, and every form now reads operands the same way

`∪` `∩` `⊎` `⊍` `∖` `⊖` (and their ASCII spellings) each had several
bodies (#9451). The `SetUnion`, `SetAddition` and `SetIntersect` opcodes
carried their own inline Set/Bag/Mix algebra. The `[op]` and
`&infix:<op>` forms went through `apply_set_union` / `apply_set_addition` /
`apply_set_multiply` / `set_diff_values` / `set_intersect_values` /
`set_sym_diff_values`. Underneath those sat seven separate ways of reading an
operand as a Bag or a Mix.

Those readings disagreed. Take the list `(a => 2, "b", "b")` against
`bag(<a a a b x>)`:

| code | Rakudo | mutsu before |
|---|---|---|
| `∪` | `a(3) b(2) x(1)` | `a(3) b(1) x(1)`: counted `b` once |
| `&infix:<∩>` | `a(2) b(1)` | `b(1)`: the routine form ignored the Pair |
| `∖` | `b(1)` | kept `:a(2)` and `:c(0)` as whole-Pair elements |
| `⊖` | `a(1) b(1) x(1)` | five keys, two of them whole Pairs |

Several other results were wrong in the same way:

- `bag(<a a a>) ∖ {a => True}` gave `a(3)`, not `a(2)`. The Hash on the right
  was read as a Set, so its weights were ignored.
- `(a => 2,) ≡ bag(<a a>)` returned False.
- A role mixin on the left operand was lost. `SetHash+{R} ∪ set(3)` came back
  as a plain `Set`: the mutability check did not look through the mixin, and
  nothing re-applied it.

## The one body

There is now a single set of three operand readings in
`src/runtime/utils/set_operand.rs`:

- `coerce_to_set`: read the operand as a Set.
- `operand_bag_counts`: read it as a Bag.
- `operand_mix_weights`: read it as a Mix.

All three follow `.Set` / `.Bag` / `.Mix`: a Pair contributes its value as a
weight, a Hash contributes its values, a Baggy subclass instance contributes
its data, and the `Scalar` container and any role mixin are looked through.

`set_op_values(SetOp, left, right)` in `src/runtime/utils/set_algebra.rs` is
the one algebra. It promotes both operands to the higher of their levels, reads
them there, and combines the two maps key by key.

It then gives the result the left operand's shape. Rakudo builds a same-type
result by cloning the left operand, so the left operand's mutability and its
role mixin both carry over:

- `SetHash+{R} ∪ set(3)` is a `SetHash+{R}`.
- A promotion builds a fresh value with no mixin: `SetHash+{R} ∪ bag(1)` is a
  `BagHash`.
- The one Rakudo exception is kept: `(+)` on a mutable Bag always builds a
  fresh `BagHash`.

Every form now calls `set_op_values`:

- The six opcodes, through one `exec_set_binary_op`. It first checks for a user
  override under both spellings, uniformly (ADR-0071). Before, union and
  intersection checked for an override, but addition, multiplication,
  difference and symmetric difference did not.
- `apply_reduction_op`, which serves `[op]` and `&infix:<op>`.

`(==)` now uses the same operand readings. The lazy-operand check is shared as
well, so every form throws the same `X::Cannot::Lazy` with Rakudo's message.

In total, about 2,000 lines of duplicated coercion and algebra are gone.

Regression test: `t/collections/set-bag-mix/set-operator-forms-agree.t`. It
compares the infix, `[op]` and `&infix:<op>` forms over a list of Pairs, a Hash
right operand, `(==)` with a Pair, the mixin and mutability carry-over rules,
and the lazy-operand error.
