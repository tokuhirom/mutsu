# Literal attribute names resolve once per untyped-VM site

The untyped-VM counterpart of the TRIR literal-name attribute ops landed
(ADR-0121 D3, #9291). An `nqp::getattr` / `bindattr` (and their `_i` / `_n` /
`_s` forms) or `nqp::p6bindattrinvres` site whose attribute name is a string
literal now compiles to `OpCode::NqpAttrC` instead of the generic
`OpCode::NqpOp`. That covers mainline code, method bodies and every routine
TRIR does not accept.

## What changed

- **The name.** The site carries an `NqpAttrSite` (`src/runtime/nqp_attr.rs`).
  It holds the name already interned and twigil-stripped, the typed form's
  conversion, and whether the op answers the value or the invocant. The
  literal is no longer pushed, re-stringified and stripped on every
  execution. The op-table walk is gone as well.
- **The class operand.** A bareword class operand (`IB` in
  `nqp::getattr($o, IB, '$!a')`) no longer compiles to a `GetBareWord` that
  runs the whole term-resolution chain on every execution. It is folded into
  the site as the same `ClassOperandSite` TRIR uses. That site remembers the
  resolution for one registry write generation. The attribute ops ignore the
  operand's value, so only its effect is owed.
- **Everything else is unchanged.** The operands are normalized exactly as
  `NqpOp` normalizes them: `VarRef` unwrap, `Proxy` FETCH, the one-shot
  callsite-line clear, `use fatal`, and the resume point on error. A site
  whose name is computed keeps the generic op.

## A shared bug this exposed

The plain-instance fast path, which is shared with TRIR's `GetAttrC` /
`BindAttrC`, answered `$!storage` / `$!reified` from the instance's attribute
store. A `Map` or `List` subclass keeps its element store outside that store,
so `nqp::getattr($map-subclass, Map, '$!storage')` read `Nil`.
`t/vm/nqp-attr-ops-slots.t` caught this as soon as the untyped path used the
fast path. `NqpAttrName` now records at compile time that a name is a
container's element store, and such names always take the generic body on
both paths.

## Measured

Callgrind instructions per op above an empty mainline `while` loop, taken from
the difference between 1 and 10,001 iterations, on release builds of `main`
and this branch:

| op | before | after |
|---|---:|---:|
| `nqp::getattr($o, IB, '$!a')` | 4,796 | 1,625 |
| `nqp::bindattr($o, IB, '$!a', 1)` | 4,709 | 1,541 |
| `nqp::getattr_i($o, IB, '$!n')` | 4,814 | 1,621 |
| `nqp::getattr(%h, Map, '$!storage')` | 1,779 | 1,532 |

Most of what remains is the untyped VM's own per-op overhead. #9291's close
condition (every row within 2x of rakudo) is not met yet.
