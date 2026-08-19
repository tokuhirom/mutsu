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
- WhateverCode such as `* + 1` — **designed, see
  [ADR-0033](../../docs/adr/0033-whatever-priming-leaf-and-derived-scope.md)**
- code-block interpolation
- regexes

Pick these deliberately by user impact rather than treating them as another
cadence of mechanical slices. Lower through the existing internal AST and
compiler; do not add a second execution engine.

### Designed: WhateverCode (ADR-0033)

`* + 1` was picked first because it is the highest-frequency construct on the list
(`.map(* + 1)`, `.grep(* > 3)`, `@a[* - 1]`) and because investigating it surfaced a
second, independent defect that shares the same root cause: mutsu primes straight
through the thunky operators, so `(* > 3 && * < 8).arity` is `2` where raku says `1`,
`(1..10).grep(* > 3 && * < 8)` returns `5 6` where raku returns `1..7`, and a ternary
primes nothing at all. Both follow from mutsu building the `WhateverCode` closure
eagerly in the parser at ~50 call sites, which destroys the pre-curry expression before
the RakuAST converter can see it and leaves no single owner for the priming-scope rule.

[ADR-0033](../../docs/adr/0033-whatever-priming-leaf-and-derived-scope.md) proposes
Rakudo's model — a leaf split (`Expr::Whatever` value vs `Expr::WhateverArg` argument),
an `Expr::WhateverCurry` scope marker, closure construction moved to the compiler, and a
single `whatever_curry::plant` shared by the parser and `rakuast::lower` — in four
phases, the first of which is behaviour-preserving.

The remaining items on both lists above are still undesigned.

## Macros

Macros, `quasi`, and unquoting depend on the construction and lowering layers.
They remain a separate future campaign and may be deferred indefinitely until a
real use case justifies it.
