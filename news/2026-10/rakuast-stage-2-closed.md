# RakuAST Stage 2 is closed; reading (A) and Stage 4 become independent work

ADR-15334 Amendment 5 records that Stage 2 of the RakuAST frontend campaign (ADR-10723) is complete under
reading (B): the parser has no `SyntheticBlock` producer, `convert` has no recognizer left, the executing
path builds no `RakuAstNode`, and `passthrough` / `ParsedLeaf` are gone. The cost target is met (zef-cli at
104.3% and the grammar-cursor test at 108.1% of the pre-flip frontend).

The three `check-frontend-debt` ceilings (10 synthetic-block sites, 38 internal-name literals, 6761 lines of
`convert.rs`) are now pinned instead of driven to zero, since they count executable IR and the `.AST` view.
Inline lowering at build time and folding the convert coverage mode into the `.AST` mode continue as their own
issues; replacing the compiler-consumed spellings by typed IR and the Stage 4 slang, RakuDoc and Deparse work
are no longer part of the #7564 campaign.
