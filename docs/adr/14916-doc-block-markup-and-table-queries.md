# ADR-14916: RakuAST model queries answered from rakudo shapes: `Doc::Block` markup/table, operator properties

- **Status**: Proposed
- **Date**: 2026-11-07
- **Deciders**: Claude (lane:rakuast worker 15)
- **Issue**: [#14916](https://github.com/tokuhirom/mutsu/issues/14916)

## 1. Context

Two `RakuAST::Doc::Block` queries had no mutsu answer. Measured with the bare `raku` 2026.09:

- `allowed-markup` answers a *type object*: `RakuAST::Doc::Block::NOK` for the block types `code`,
  `implicit-code` and `defn`, `RakuAST::Doc::Block::OK` for every other type. Both are `Map`
  subclasses (`.^mro` is `OK, Map, Cool, Any, Mu`); `OK<X>` is `True` and `NOK<X>` is `False` for
  any key, and `.defined` is false.
- `visual-table` is `True` for a `table` block holding a `RakuAST::Doc::LegacyRow`, `False` for a
  `table` block without one, and the junction `any(False, False)` for a block of any other type.

## 2. Decision

1. `visual-table` and the choice between `OK` and `NOK` are native method rows over the node's
   `type` and `paragraphs` fields (`src/rakuast/doc_block_queries.rs`).
2. `OK` and `NOK` are real Raku classes (`is Map`, `AT-KEY` answering `True` / `False`) declared by
   a small unit prelude, injected when the unit's text names `allowed-markup` or
   `RakuAST::Doc::Block::`. A native substitute for a `Map` subclass would need a second class
   registry path for no gain; a prelude class shows up in `.^mro`, `~~ Map` and `.^name` exactly
   as rakudo's does, and costs nothing for a program that never asks.

## 3. Consequences

An `EVAL($ast)` unit that calls `allowed-markup` without the text naming it does not get the
prelude (its type object then has no `AT-KEY`); that needs the unit-level gate the deparser's
prelude has (`inject_deparse_prelude_for_unit`) and is left for when a consumer needs it.

## 4. Operator properties (amendment)

`properties`, `reducer-name` and `default-operator-properties` of the operator nodes answer an
`OperatorProperties` instance (rakudo's class: `precedence`, `sub-precedence`, `associative`,
`thunky`, `dba`, `next-term`, `iffy`, `diffy`, `fiddly`, `adverb`, `ternary`; `.raku` omits empty
values and prints flags as `:name`).

- The class is a Raku prelude (`run_prelude_operator_properties.rs`), injected when a unit names
  `OperatorProperties` or `.properties`; the native rows build an instance of it.
- The data is the existing built-in precedence table (`src/op_prec`, generated from `Routine.prec`),
  plus what the grammar parses without a routine (`:=`, the flip-flops, the dotty infix) and the
  few symbols rakudo's `OperatorProperties` does not know although `Routine.prec` does (`÷ ≠ ≤ ≥ ∊ ∍
  ⩵ ⩶`, prefix `−`; the lone prefix `//` it does). Each difference was found by probing every table
  symbol on rakudo 2026.09.
- `reducer-name` is chosen by associativity (and the `list-infix` level); `MetaInfix::Assign`
  answers the item- or list-assignment level (list when the inner infix binds looser than
  assignment) but reduces like its inner infix, `Reverse` flips `left`/`right`, `Negate` and
  `Hyper` answer the inner infix's, `Cross`/`Zip` are the list-infix level, a `FunctionInfix` the
  additive level.
- Not done: the class-level lookup API (`OperatorProperties.infix("+")`, `.prefix`, `.circumfix`,
  ...).
