# RakuAST work

RakuAST work is organized by implementation scope, not in a separate
`todo/rakuast/` category. RakuAST is a reflection/model layer over mutsu's
internal `Expr`/`Stmt` AST, so each completed slice should preserve the normal
`Parser -> Compiler -> VM` pipeline.

## Where work lives

- `todo/deep/rakuast-remaining.md` is the campaign overview and records broad
  representation gaps.
- `todo/tickets/rakuast-<slug>.md` contains a self-contained implementation
  slice that can be completed in one PR.
- `todo/deep/rakuast-<slug>.md` contains a slice that needs an ADR, parser or
  internal-AST redesign, or a broader execution campaign.
- `docs/adr/` contains architectural decisions and their current phase/status.
- `t/rakuast-<slice>.t` is the focused dual-oracle regression test.
- `news/YYYY-MM/` records completed slices after they merge.

The per-slice todo or news entry is the source of truth. The campaign overview
should remain an index rather than a second detailed ledger, so small slices do
not all need to edit the same shared file.

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
