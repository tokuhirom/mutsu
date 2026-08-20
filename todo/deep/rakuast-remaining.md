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
- WhateverCode such as `* + 1` — **Phase 1 (deferral) and Phase 2 (RakuAST read / the
  leaf split) shipped: `Q[* + 1].AST` works now. Phase 4 (the thunk-barrier priming
  correctness fix — the highest-payoff remaining slice, see below) is next; Phase 3
  (RakuAST write / `EVAL`) not started — see
  [ADR-0033](../../docs/adr/0033-whatever-priming-leaf-and-derived-scope.md)**
- code-block interpolation
- regexes

Pick these deliberately by user impact rather than treating them as another
cadence of mechanical slices. Lower through the existing internal AST and
compiler; do not add a second execution engine.

### Phases 1-2 shipped, Phase 4 is the next highest-payoff slice: WhateverCode (ADR-0033)

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

Phase 1 (the behaviour-preserving deferral) shipped 2026-08-19: `src/whatever_curry/`
now owns closure construction, invoked from a single `Expr::WhateverCurry` compiler arm;
the parser's ~50 `wrap_whatevercode` call sites construct that marker instead of building
the closure eagerly, verified zero-behaviour-change against the full `t/` + targeted
roast suites. See the ADR's own Outcome section for the full list of latent-bug fixes
this deferral surfaced along the way.

Phase 2 — the *leaf* half (`Expr::WhateverArg` → `RakuAST::WhateverCode::Argument`) —
shipped 2026-08-20 (same day as its design): `src/whatever_curry/mark.rs` classifies
every `*` leaf post-parse per the ADR's raku-measured table, `Q[* + 1].AST` now renders
correctly, and the change was verified behaviour-preserving by construction (`is_whatever`
treats both leaf variants identically everywhere outside `src/rakuast/`) plus a new
dual-oracle `t/rakuast-whatever-code.t` (68 assertions, passes verbatim under mutsu and
the system raku). See the ADR's "Phase 2 outcome" section for the full change list,
including two adjacent RakuAST operator-name rendering bugs (`!~~`, `=>`) it fixed along
the way and one latent runtime bug it fixed as a side effect (`$_ ~~ *` previously
shadowed the caller's topic instead of reading it dynamically).

**Phase 4 (the thunk-barrier priming correctness fix) is now the highest-payoff remaining
slice** — it is a genuine, user-visible correctness bug independent of RakuAST
(`(1..10).grep(* > 3 && * < 8)` silently returns the wrong list, `5 6` instead of raku's
`1 2 3 4 5 6 7`), not merely a `.AST` gap, and Phase 2's leaf split is its prerequisite
(the ADR's `plant()` scope authority needs to read the classified leaves). See the ADR's
"Phasing" section for the exact scope (thunk barriers: `&&`/`||`/`//`/`and`/`or`/
`andthen`/`orelse`/`notandthen`/ternary) and its "Phase-4 prerequisite" section (the
chained-comparison `&&`-duplication trap that must be resolved first, e.g. via a new
`Expr::ChainedCompare` node). Phase 3 (RakuAST write / `EVAL` of a hand-constructed
`WhateverCode::Argument` tree) remains designed only at the ADR's outline level and has
no roast/correctness payoff of its own (RakuAST has zero roast dependents, ADR-0011
ANALYSIS §7-9) — pick it up after Phase 4, not before.

The remaining items on both lists above are still undesigned.

## Macros

Macros, `quasi`, and unquoting depend on the construction and lowering layers.
They remain a separate future campaign and may be deferred indefinitely until a
real use case justifies it.
