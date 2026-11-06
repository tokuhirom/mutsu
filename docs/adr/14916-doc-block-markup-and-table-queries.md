# ADR-14916: `Doc::Block.allowed-markup` is a `Map` type object from a Raku prelude; `visual-table` is native

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
