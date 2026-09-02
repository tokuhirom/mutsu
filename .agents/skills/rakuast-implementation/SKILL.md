---
name: rakuast-implementation
description: Implement or investigate mutsu RakuAST compatibility slices using Rakudo oracle measurements, bidirectional conversion, lowering, and focused regression tests.
metadata:
  short-description: Work through a bounded mutsu RakuAST compatibility slice
---

# RakuAST implementation

Use this skill for work involving `src/rakuast/`, `t/rakuast*.t`,
`todo/deep/rakuast-remaining.md`, or RakuAST-specific ADR/design work.
It covers the RakuAST-specific investigation and implementation details; use
the repository's normal ticket and PR workflow for branching, validation, and
publication.

## Establish the slice

Before changing code:

1. Read `AGENTS.md`, the selected todo entry, every linked ADR/design document,
   and the affected parser, internal AST, compiler/VM, and tests. Re-check ADR
   status lines against the current tree.
2. Identify the existing `Expr`/`Stmt` representation and whether ordinary
   execution already works. Prefer a slice where the parser and execution path
   exist and only the RakuAST boundary is missing.
3. Probe a minimal equivalent program with the bare `raku` executable. Record
   the node class names, fields, omitted defaults, accessor results, constructor
   shape, and the result of `EVAL($ast)`.

Treat the Rakudo result as an oracle for the measured construct, not as proof
that arbitrary RakuAST programs are equivalent.

## Implement both directions

RakuAST is a model layer over the existing execution pipeline:

- In `src/rakuast/convert.rs`, map the internal AST to the measured RakuAST
  shape. Do not reconstruct distinctions that the parser or internal AST has
  already discarded; preserve them in the parser/internal representation or
  file a design/deep item first.
- In `src/rakuast/mod.rs`, add class names, registry entries, constructor
  metadata, and accessors together. Match Rakudo's positional versus named
  fields and omitted false/default values.
- In `src/rakuast/lower.rs`, accept the supported hand-built RakuAST shape and
  lower it to the existing `Expr`/`Stmt` path. Reuse the compiler and VM.
- Do not add a second interpreter or a VM/runtime slow path such as
  `call_method_with_values`, `run_instance_method`, or `eval_block` to make the
  RakuAST case work.

If a feature needs a new semantic distinction, make that a bounded parser /
internal-AST design slice and link its ADR rather than guessing in the
converter.

## Test the contract

Add a focused `t/rakuast-<slice>.t` test with three kinds of assertions:

- read direction: measured node classes, accessors, and relevant field
  omission/presence;
- write direction: `EVAL` of the hand-built or round-tripped AST;
- semantics: strict and flag/adverb variants that exercise the existing
  execution path.

Prefer structural assertions over snapshots of an entire `.gist` when the
format contains incidental detail, while pinning exact field names and values
where they are part of the compatibility contract. Keep probes minimal and
English comments/test descriptions. For an individual roast test use
`MUTSU_FUDGE=1`; never set it for ordinary Raku programs.

## Track the campaign

Do not create a fourth `todo/rakuast/` category. Keep the campaign overview in
`todo/deep/rakuast-remaining.md`, put a self-contained implementation slice in
`todo/tickets/rakuast-<slug>.md`, and keep architectural blockers in
`todo/deep/rakuast-<slug>.md`. Use `docs/rakuast/README.md` for stable workflow
guidance, ADRs for decisions, `news/YYYY-MM/` for completed work, and the
focused test as the executable record. Avoid making every small slice edit a
shared campaign ledger; the per-slice todo/news file is the source of truth.

For an implementation PR, follow the repository's required full validation and
inspect `tmp/make-test.log` and `tmp/make-roast.log` before publication.
