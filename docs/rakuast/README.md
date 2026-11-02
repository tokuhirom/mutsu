# RakuAST work

RakuAST work is organized by implementation scope, not in a separate
RakuAST-only label. RakuAST is a reflection/model layer over mutsu's
internal `Expr`/`Stmt` AST, so each completed slice should preserve the normal
`Parser -> Compiler -> VM` pipeline.

## Where work lives

- The `todo:deep` "RakuAST remaining work" issue is the campaign overview and
  records broad representation gaps.
- A `todo:ticket` issue contains a self-contained implementation slice that can
  be completed in one PR.
- A `todo:deep` issue contains a slice that needs an ADR, parser or
  internal-AST redesign, or a broader execution campaign.
- `docs/adr/` contains architectural decisions and their current phase/status.
- `t/rakuast-<slice>.t` is the focused dual-oracle regression test.
- `news/YYYY-MM/` records completed slices after they merge.

The per-slice issue or news entry is the source of truth. The campaign overview
should remain an index rather than a second detailed ledger, so small slices do
not all need to edit the same shared file.

## Finding the next slice

The slices are planned on the campaign issue (#7564, "Stage 1 slice plan"); take
the next one from there. `scripts/rakuast-frontend.sh eval-update` runs every
`t/` file as `EVAL(slurp($file).AST)` and adds the new passes to
`ci/rakuast-ast-eval-passing.txt`; the files outside that list are the gaps in
the write direction (run one with `scripts/run-t-ast-eval.sh` to see why it
fails). A file that reads `$?FILE` names the driver and can never join.

## Slice checklist

For each construct, investigate the smallest useful program under both a bare
`raku` and mutsu:

1. Measure the Rakudo `.AST` class tree, field names, omitted defaults,
   accessors, constructor shape, and `EVAL($ast)` result.
2. Locate the existing parser/internal `Expr` or `Stmt` and confirm ordinary
   execution behavior.
3. Implement the read direction in `src/rakuast/convert.rs`.
4. Add or complete class, constructor, and accessor metadata in
   `src/rakuast/mod.rs`.
5. Implement the write direction in `src/rakuast/lower.rs`, lowering into the
   existing compiler/VM path.
6. Add read, write, and semantic assertions in a focused `t/rakuast-*.t` file.
7. Update the relevant ADR, campaign entry, or news record only with measured
   facts.

If the parser/internal AST has already erased a distinction, do not guess it
inside RakuAST conversion. Preserve the distinction upstream or turn it into a
design/deep slice first. Do not add an alternate interpreter or a VM/runtime
slow path for RakuAST.

The reusable agent procedure for this work is in
`.agents/skills/rakuast-implementation/SKILL.md`.

## Measuring `.AST` text parity

`scripts/ast-text-corpus.sh` measures how close mutsu's `.AST` text is to
rakudo's over a sample of the test suite (every 8th `t/**/*.t`): rakudo and mutsu
each render `slurp($file).AST`, one `.raku` text per top-level statement, and the
statements are compared. `compare` prints the share of identical statements and
the differing ones by class; `show CLASS` prints example hunks. A file mutsu
refuses to convert is not compared (its message is in `tmp/ast-text/mutsu/*.err`).

The distinctions the parser discards (`foo 1` vs `foo(1)`, `:a(1)` vs `a => 1`,
`^5` vs `0 ..^ 5`, `.say` vs `$_.say`, `%h<a>` vs `%h{'a'}`, `so` vs `?`) are kept
in fields the compiler ignores: `listop` on `Expr::Call` / `Stmt::Call`,
`Binary.form`, `MethodCall.sugar`, `Index.spelling`, `IndexAssign.spelling`,
`CallArg::Named.form` and `Unary.word`. A pass that rebuilds one of these nodes
must carry the field through (`wrap_composition_operands` once dropped them all).

