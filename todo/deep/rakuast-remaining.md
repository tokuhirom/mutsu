# RakuAST remaining work

RakuAST is a reflection/model layer bidirectionally convertible with mutsu's
internal `Expr`/`Stmt` AST, not a frontend rewrite. The fixed design and phasing
are in [ADR-0011](../../docs/adr/0011-rakuast-model-layer-and-phasing.md).
Completed read, introspection, construction, and EVAL slices are recorded in
[the July 2026 news](../../news/2026-07.md) and the individual
`news/2026-07/rakuast-*.md` entries.

## Read-direction representation gaps

Several source constructs are desugared or lose distinctions before the RakuAST
conversion sees them:

- `.=` and compound assignment
- hyper operators other than the supported hyper-method form
- signature return types
- anonymous subs with explicit signatures
- `with` / `without`
- grammar declarations
- associative `%h{...}` versus `%h<...>` subscripts

Recovering these requires preserving the distinction in the parser/internal AST,
not guessing during RakuAST conversion.

## Type-object introspection

The RakuAST type registry supports the common hierarchy, method, attribute, and
`.^can` operations. Audit and implement the remaining metaobject operations from
the same model metadata. Do not expose Rakudo compiler-private `IMPL-*` details.

## Construction

Advanced parameter construction remains:

- sub-signatures
- type captures
- array shapes
- signature constraints

These must validate, render, expose through introspection, and lower through
`EVAL` consistently with the already-supported parameter forms.

## Lowering and EVAL

The remaining constructs are blocked by representation mismatches and each needs
an explicit design:

- placeholder blocks such as `{ $^a }`
- `with` / `without`
- list assignment
- `constant`
- associative subscripts
- `CATCH` blocks
- WhateverCode such as `* + 1`
- code-block interpolation
- regexes

Pick these deliberately by user impact rather than treating them as another
cadence of mechanical slices. Lower through the existing internal AST and
compiler; do not add a second execution engine.

## Macros

Macros, `quasi`, and unquoting depend on the construction and lowering layers.
They remain a separate future campaign and may be deferred indefinitely until a
real use case justifies it.
