# ADR-10723: RakuAST is mutsu's frontend IR — the parser emits RakuAST, the internal AST becomes the lowered form

- **Status**: Accepted (user decision 2026-10-22; proposed 2026-10-02). Supersedes the pipeline
  decision of [ADR-0011](0011-rakuast-model-layer-and-phasing.md). The §4 questions stay gates
  before Stage 2; the source-position question is decided (2026-10-22); the representation
  question and the Stage 2 mechanism are Proposed (2026-10-23, §4).
- **Date**: 2026-10-02
- **Deciders**: tokuhirom, Claude
- **Issue**: [#10723](https://github.com/tokuhirom/mutsu/issues/10723). Roadmap:
  [#7564](https://github.com/tokuhirom/mutsu/issues/7564).
- **Related**: [ADR-0011](0011-rakuast-model-layer-and-phasing.md) (the reflection-layer design this
  replaces), [ADR-0088](0088-rakuast-regex-boundary-tree.md) (the regex source tree),
  [ADR-0026](0026-slang-activation-architecture.md) / [ADR-0091](0091-slang-package-declarators.md) /
  [ADR-0098](0098-if-pragma-actions-slang.md) (slang activation by interpretation),
  [ADR-0033](0033-whatever-priming-leaf-and-derived-scope.md) (WhateverCode priming, already
  shared by the parser and `rakuast::lower`), [ADR-0137](0137-typed-ast-visitor-for-analyses.md)
  (the typed visitor over the internal AST)

## 1. Context

### 1.1 Rakudo changed underneath ADR-0011

ADR-0011 (2026-07-18) made RakuAST a *reflection/model layer*: the parser builds mutsu's internal
`Expr`/`Stmt` tree, `.AST` converts that tree into RakuAST nodes, and `EVAL($ast)` lowers nodes
back. It rested on two stated premises:

1. "mutsu's frontend is not built that way and **must not be rebuilt** … no near-term payoff and
   enormous risk", and
2. "no roast file and no bundled battery consumes this layer, so its remaining work has **no
   downstream dependency**" — RakuAST was "a deliberate new capability direction".

Both are now false:

- **Rakudo 2026.09 made the RakuAST implementation of the Raku grammar and actions the default
  compiler frontend** — the result of roughly 5000 commits since 2020. The legacy grammar is
  reachable only by opting out (`RAKUDO_LEGACY=1` / `RAKUDO_RAKUAST=0`) and **is removed when the
  6.e language level is released**.
  ([release notes](https://github.com/rakudo/rakudo/releases/tag/2026.09),
  ["Mainstreaming RakuAST"](https://dev.to/lizmat/mainstreaming-rakuast-49j8))
- What the ecosystem is written against moves with it. Slangs must re-target the RakuAST grammar's
  hooks (that is what Slangify abstracts). L10N slangs parse localized source to RakuAST and
  print it back with `RakuAST::Deparse::L10N::*`. RakuDoc v2 is `RakuAST::Doc` and its renderer
  (`Rakuast::RakuDoc::Render`, which the `Elucid8::Build` / `Air-Plugin-RakuDoc` documentation
  toolchain depends on) is a RakuAST walker. `.DEPARSE` is the round-trip contract.
- **`macro` / `quasi` are not supported in 6.e.** ADR-0011's Phase 6 has no target.

The `ecosystem/` parity corpus already holds 29 distributions that use `RakuAST` or `.AST`
directly (2026-10-01 sweep): 15 green, 1 partial, 7 red, 3 `blocked_dep` on
`RakuAST::Deparse::Highlight`, 3 without a usable baseline. That number only grows as module
authors adopt the 6.e frontend.

### 1.2 The reflection layer's structural cost

Two months of slices show what the reflection design costs. The parser desugars as it parses:
`with` becomes a temp variable plus `if .defined`, `$x .= m` becomes the same tree as
`$x = $x.m`, `%h<a>` and `%h{"a"}` become the same `Expr::Index`, `my ($a, $b) = …` becomes a
temp-var destructuring, `-<<@a` becomes a `__mutsu_hyper_prefix` call, and so on. By the time
`.AST` runs, the distinction raku keeps is gone, so every RakuAST node that differs from rakudo's
needs **a new marker smuggled through the internal AST** (`src/parser/` holds roughly 300
`"__…"` internal-name string literals, many of them such markers) and **a converter arm that reverse-engineers the desugaring**
(`src/rakuast/convert.rs` is 4k lines; `lower.rs` another 3k). Each of these is a separate
ticket — #10653, #10654, #10655 are the latest three — and the campaign has no end state:
"renders the same node as rakudo for an arbitrary program" is unreachable by construction,
because the information is destroyed before conversion.

The two directions also drift independently: a construct can be readable but not lowerable, or
lowerable only from a hand-built tree, and EVAL of a converted tree is a *second* frontend path
that ordinary programs never exercise.

### 1.3 Baseline: how far the round trip gets today (2026-10-02)

Measured on every one of the 5638 `t/**/*.t` files with a release build of `main` (`516446ee`, 2026-10-01),
comparing a normal run against `EVAL(slurp($file).AST)` (exit status and the number of `ok`
lines must match). All 5638 pass when run normally.

| Measure | Files | Share |
| --- | ---: | ---: |
| `slurp($file).AST` succeeds on the file as written | 1 | 0.0% |
| … after deleting the `use Test;` / `use v6…;` / `use lib …;` lines (the driver loads `Test`) | 939 | 16.7% |
| … and `EVAL` of that tree reproduces the normal run | 485 | 8.6% |
| … but `EVAL` of that tree runs **differently** from the normal run | 454 | 8.1% |

The first row is a single refusal: `use Test;` (a `Stmt::Use` that `convert` does not model) stops
almost every test file. Past it, the most frequent refusals (first refusal per file) are an
unresolved bareword (964 — mostly `is` / `done-testing` once `use Test` is gone, an artifact of
the stripping), the remaining `use` statements (453), literal values the parser pre-computed
(413 — `Any` as a type literal, `Inf`, `Slip`), the parser's `SyntheticBlock` (251) and
`IndexAssign` (250) desugarings, non-trivial signature parameters (192), `__mutsu_*` desugar
markers (151), and attributes / methods / declarations with traits (146 / 130 / 121).

The last row matters most: **for almost half of the files that do convert, the converted tree
silently means something else.** One root cause, found while sampling: a bare block in statement
position (`{ say "blk" }`) lowers to a closure *value* that is never called, so its body does not
run; rakudo prints `blk`. Nothing reports such a divergence today, because no ordinary program
runs through `lower`. This is what Stage 0 (§2.2) is for.

## 2. Decision

### 2.1 The target pipeline

```
 Source ──► Parser ──► RakuAST tree ──► lower ──► Expr/Stmt ──► Compiler ──► bytecode ──► VM
             ▲          (canonical)    (the only    (lowered IR,
             │            │  ▲          desugaring)  the QAST analogue)
   slang grammar/actions  │  └── RakuAST::*.new(...), slang actions, EVAL($ast)
                          └────► .AST / .DEPARSE / introspection (no conversion step)
```

- **The RakuAST tree is the canonical frontend representation.** The parser produces it; `.AST`
  returns it; `EVAL($ast)`, hand-built trees and (eventually) slang actions feed the *same*
  `lower`. There is one way into the compiler, so a construct that runs is a construct that
  round-trips.
- **`lower` is the only place desugaring happens.** The parser stops desugaring. `with`,
  `.=`, list-assignment destructuring, hyper prefix, `%h<…>` vs `%h{…}` and the rest stay
  distinct in the tree and are expanded by `lower`. Parser markers whose only purpose was to let
  `convert` undo a desugaring are deleted as their construct moves.
- **`Expr`/`Stmt` stays — demoted to the compiler's input.** It plays QAST's role in Rakudo: a
  lowered, compiler-oriented form. The compiler, the VM, TRIR, the JIT, the typed visitor
  (ADR-0137) and every analysis over the internal AST keep working on it. This ADR does **not**
  rewrite the compiler or add an execution engine (AGENTS.md hard rule), and it does not move
  compile-time analyses onto RakuAST.
- **`src/rakuast/convert.rs` is retired.** Once the parser emits RakuAST for a construct, the
  internal-AST → RakuAST arm for it is dead code and is deleted. When the last arm goes, the
  module goes.

### 2.2 Migration strategy — Rakudo's own

Rakudo did not flip in one commit: it ran the RakuAST frontend behind `RAKUDO_RAKUAST=1` for
years, published pass counts (`make test` 146/166, `make spectest` 1331/1350 in March 2025), and
flipped the default only at parity. mutsu does the same, and the pass count is the roadmap's
metric.

- **Stage 0 — round-trip frontend mode.** `MUTSU_RAKUAST=1` runs every compilation unit (the
  main program, modules, `EVAL` strings) through `parse → convert → lower → compile` instead of
  `parse → compile`. This needs no new representation: both directions exist today. A refusal in
  `convert` or `lower` is a compile error in this mode, never a silent fallback. A
  `scripts/rakuast-frontend-count.sh` (or a `scripts/dev` job) reports the number of `t/` and
  whitelisted roast files that pass in the mode, and CI records it as a **ratchet** (it may only
  grow). Every failure is, by construction, a place where the two directions disagree.
- **Stage 1 — close the round trip.** Work the Stage 0 failures by *moving desugaring from the
  parser into `lower`* (or into the tree the parser keeps), never by teaching `convert` another
  reverse-engineering trick. The `#10653`-style tickets are absorbed here: each fix removes a
  parser desugaring instead of adding a marker.
- **Stage 2 — the parser emits RakuAST.** Construct family by construct family, the parser builds
  RakuAST nodes directly and the matching `convert` arms are deleted. The mixed state is
  well-defined because a RakuAST node and a not-yet-migrated internal-AST subtree can both reach
  `lower` (a transitional `RakuAST`-wraps-`Expr` leaf, never visible to user code, deleted at the
  end of the stage).
- **Stage 3 — flip the default.** When the Stage 0 count equals the ordinary-frontend count on
  `make test` and `make roast`, and the frontend-cost budget (§2.4) holds, `MUTSU_RAKUAST=1`
  becomes the default with `MUTSU_LEGACY=1` as the opt-out for one release. Then the legacy path,
  `convert.rs` and the remaining parser desugarings are deleted.
- **Stage 4 — what the new frontend unlocks.** Run in parallel with Stage 2 where it does not
  depend on it, otherwise after:
  - **Slangs by execution.** ADR-0026 §4 deliberately *interprets* a slang's token and action
    names instead of executing them, because mutsu has no grammar to mix into. With RakuAST as the
    output of parsing, a slang action that builds RakuAST nodes has somewhere to put them. Which
    of the RakuAST grammar's hooks mutsu exposes, and how, is a follow-up ADR that supersedes
    ADR-0026 §4; it is out of scope here.
  - **RakuDoc v2** as `RakuAST::Doc` nodes produced by the parser (`$=rakudoc`), enough for
    `Rakuast::RakuDoc::Render`.
  - **`.DEPARSE`** as a total function over the tree (the L10N deparsers, `RakuAST::Deparse::Highlight`).

Each stage is a set of ordinary PRs that pass `make test` / `make roast` on the default frontend;
nothing is gated on a big-bang switch.

### 2.3 What is dropped

- **Macros, `quasi` and unquoting (ADR-0011 Phase 6)** — Rakudo drops them in 6.e. Not planned;
  recorded on its own as [ADR-9885](9885-experimental-macros-not-planned.md) (Accepted).
- **"One more argument form per PR" campaigns.** ADR-0088's regex tree and the #8033 slices
  stay valid (the regex source tree is exactly the kind of non-lossy representation this ADR
  wants), but new regex slices are chosen by Stage 0/1 failures and by consumers, not by
  enumerating every colonpair spelling.

### 2.4 Frontend cost budget

Building a RakuAST tree and lowering it is an extra pass over every compilation unit. Stage 3
may not flip the default unless the total frontend time (parse + build + lower) on the
`perf-tuning` compile-heavy cases is within **10%** of the legacy frontend, measured per the
`perf-tuning` skill. If it is not, the representation question in §4 is answered by measurement
before the flip, not after.

## 3. Consequences

- **The roadmap gets a number.** "How far along is RakuAST?" is answered by the Stage 0 pass
  count against the ordinary-frontend count, the way Rakudo answered it.
- **The ticket stream changes shape.** Read-direction "renders the wrong node" tickets stop being
  converter work: each one is either a Stage 1 desugaring move or disappears when its construct
  reaches Stage 2.
- **`EVAL($ast)` stops being a side path.** Every program exercises `lower`, so the write
  direction gets the full test suite's coverage instead of the dual-oracle files'.
- **Precompilation** keeps serializing the lowered `Vec<Stmt>` (ADR unchanged); a precompiled
  module does not need its RakuAST tree unless `.AST` of a routine is asked for, which is a
  separate, later question.
- **Risk**: this is a frontend rewrite spread over many PRs. It is mitigated by the stage gates —
  the default frontend is never switched before parity — and by the fact that the compiler, VM and
  every analysis are untouched.
- ADR-0011's representation (`Value::RakuAst`, `RakuAstClass`, the field table, the type-object
  registry, construction) is kept; its *pipeline* decision and Phase 6 are superseded.

## 4. Open questions

- **Typed nodes vs. the uniform `RakuAstNode`.** Today a node is a `RakuAstClass` plus a
  `Vec<RakuAstField>` of `Value`s — right for reflection, unproven as parser output (allocation,
  `Value` construction at parse time, no typed access in `lower`). Stage 0/1 can run on it.
  Before Stage 2, measure parse-time cost on the compile-heavy cases and decide between keeping
  it, or a typed Rust tree with `Value::RakuAst` as a view over it.

  **Proposed (2026-10-23, [#11413](https://github.com/tokuhirom/mutsu/issues/11413)): typed
  parser nodes, with the uniform node as their view.** This also fixes the Stage 2 mechanism
  that §2.2 left open.
  - **The parser builds a typed node per migrated construct family.** It is a Rust struct in
    `src/ast/parsed/` shaped like the RakuAST node (`Statement::If` has its condition, its
    `then` block, its `elsif`/`orwith` clauses and its `else` block). Its children that have
    not migrated yet are ordinary `Expr`/`Stmt` values; this is §2.2's transitional "RakuAST
    wraps `Expr`" leaf. Each node that Rakudo gives an origin has a position member, recorded
    by the parser as it builds the node. Building one allocates no `Value`.
  - **One lowering, in `rakuast`.** A single function lowers a typed node to the `Expr`/`Stmt`
    the compiler runs. That function holds the desugaring the parser used to do inline. The
    uniform node's lowering (`EVAL($ast)`, hand-built trees, the `MUTSU_RAKUAST` round trip)
    reads the uniform node into the same typed struct and calls the same function. There is
    never a second lowering of a family.
  - **The uniform `RakuAstNode` is the view.** `.AST` renders the typed node into the uniform
    node, positions included. That arm is not a reverse-engineering `convert` arm: it reads a
    tree the parser built in RakuAST's shape. The `convert` arms that undid the family's
    desugaring are deleted once the parser no longer emits the desugared shape for that family.
  - **Where the typed node lives.** Every parse builds it. An executing parse (the main
    program, modules, `EVAL` strings, the lazily parsed fragments the compiler asks for)
    lowers it where it is built, so every tree that reaches the compiler, the precompilation
    cache or an analysis is the tree those passes see today. A parse whose tree only the RakuAST
    conversion reads (`Str.AST`, a `MUTSU_RAKUAST` unit: ADR-12199's `keep_spelling` parses)
    keeps it, in one transitional carrier per syntactic category (`Stmt::Ast`, later
    `Expr::Ast`). The carrier is deleted at Stage 3, together with `convert`. Positions are
    recorded the same way in both kinds of parse: the node is the same, and only where it is
    lowered differs. This is not the `.AST`-only position-recording mode that the
    source-position decision below rules out.
  - **Order.** Families migrate one PR each. Statement-level families come first: the
    conditional family, then loops and bare blocks, then routine declarations. Their nodes are
    few per program, and their blocks carry most of the `Block`/`Blockoid`/`StatementList`
    origins. Expression families follow. For a family whose `Expr` is already lossless, the
    typed node is that `Expr` plus its positions.
  - **Cost.** Building the typed node allocates no `Value`, and lowering it moves its
    children. The executing path therefore adds one struct per migrated construct over the
    parser's inline desugaring. Each family PR measures parse time on the `perf-tuning`
    compile-heavy cases before it lands. §2.4's 10% budget is checked against the sum of those
    measurements.

  Rejected:
  - **Building uniform `RakuAstNode`s at parse time.** Each node costs a `Value` allocation
    per field, and the result would be lowered at once on every executing path. Holding uniform
    nodes inside the parse tree also leaves the whole-program passes after parsing
    (`whatever_curry::mark`, `use trace` numbering, scope diagnostics) unable to reach the
    `Expr`/`Stmt` leaves behind a `Value`.
  - **A top-down tree returned by the parser** (a RakuAST root with `Stmt` leaves, lowered
    after parsing). Every block reached from an unmigrated `Expr` (closures, routine bodies)
    would have to be lowered eagerly, so origins in it would depend on how deep the migration
    had got. Every parse-time shape check over a statement list would also have to learn the
    mixed form.
  - **Span fields on the existing desugared variants** (`Stmt::If` with block spans, read by
    `convert`). The parser would keep desugaring, and `convert` would read positions off a
    desugared tree, which the source-position decision below excludes.
- **Source positions.** `RakuAstNode` carries no position; the internal AST carries line
  information as `Stmt::SetLine` markers. Error messages and `$?LINE` must not regress, so the
  tree needs positions (Rakudo keeps an `origin`) before Stage 2. Decide the representation
  before the first parser-emits-RakuAST slice.

  **Decided (user decision 2026-10-22): Rakudo-compatible character ranges.**
  - Every node may carry an origin `{ from, to }`: character offsets into the compilation
    unit's source, with the meaning Rakudo gives `RakuAST::Origin.from` / `.to`. It is a
    member of the node struct, not a `RakuAstField`, so it does not depend on the
    typed-vs-uniform node question above and never shows in a node's constructor form.
  - The source is held once per compilation unit (shared, with a line-start index) and is
    what `.origin.source` answers. `RakuAST::Origin::Source.original-line(pos)` is a binary
    search over the line starts, O(log n), n = lines in the unit.
  - Lines are derived, never stored: `lower` emits `Stmt::SetLine` and the call-site line
    markers from a node's `from`, and `$?LINE`, error positions and backtraces read the same
    value. Columns come for free.
  - The parser records the range when it builds a node (Stage 2). A node built by `convert`
    from the internal AST, or by hand, has no origin below the statement level; there is no
    second, `.AST`-only position-recording mode in the parser. Origins on nested nodes
    therefore arrive construct family by construct family as Stage 2 migrates them.
  - The line-valued statement `origin` field of Stage 1 is a transitional form: the
    statement nodes switch to the range representation with the first parser-emits-RakuAST
    slice, and `.from` / `.to` stop answering line numbers.

  Rejected: extending the line-valued field to every node (`.from` / `.to` would keep a
  meaning Rakudo does not give them, and no columns); a per-unit side table from node identity
  to range (hand-built nodes, `EVAL($ast)` and tree rewrites lose the mapping).
- **Where compile-time knowledge lives.** The parser currently resolves some names while parsing
  (declared types, imported operators, slang modes). In Rakudo that is RakuAST's resolver and
  BEGIN-time logic. Whether mutsu keeps it in the parser or moves it into `lower` is decided per
  construct in Stage 2; it must not create a second resolver.

## 5. Alternatives considered

- **Keep ADR-0011 and keep closing gaps one at a time.** Rejected: the gaps are created by the
  pipeline itself (§1.2), so the queue never empties, and the result is still a layer ordinary
  programs never run through.
- **Make the internal AST lossless instead** (stop desugaring in the parser, keep converting).
  This is Stage 1 alone. It narrows the gaps but keeps two tree types that must agree on every
  construct, a 4k-line converter, and an EVAL path that only RakuAST users exercise. Kept as a
  stage, rejected as the end state.
- **Compile directly from RakuAST and delete `Expr`/`Stmt`.** Rejected: it rewrites the compiler,
  TRIR, the JIT inputs and every analysis for no user-visible gain. Rakudo itself does not compile
  RakuAST directly either — it lowers to QAST.

## 6. Implementation status

### Stage 0 (2026-10-02, [#10733](https://github.com/tokuhirom/mutsu/issues/10733))

- `use` / `no` / `use v6.d`, statement-level calls and statement-position bare blocks cross the
  boundary (`src/rakuast/use_stmt.rs`; `news/2026-10/rakuast-use-statements-and-bare-blocks.md`).
  Before that, `use Test;` alone stopped all but 1 of 5638 `t/` files.
- The mode is `src/rakuast/frontend.rs`, with one refinement of §2.2: it has two levels.
  `MUTSU_RAKUAST=1` round-trips the program's own units (the main program and every `EVAL`
  string); `MUTSU_RAKUAST=all` also round-trips every `use`d module and `require`d file. A module
  is shared by every program that loads it, so while the bundled `Test.rakumod` does not
  round-trip, `all` fails every test file at its first line and says nothing about the files.
  `1` is the level the metric counts; `all` becomes the metric once the bundled modules pass.
- The ratchet: `ci/rakuast-frontend-passing.txt` lists the `t/` files that pass under
  `MUTSU_RAKUAST=1`; `scripts/rakuast-frontend.sh check` runs them in CI's `test-suites` job, and
  `update` grows the list (never shrinks it). First count: **984 / 5696** `t/` files.
- Not yet counted: whitelisted roast files, and the `all` level.

### Stage 1 (from 2026-10-02)

- Slices that only needed a converter/lowerer arm for a construct the parser already keeps
  distinct: subscript assignment (`assignee` / `Assignment` over a subscript, #10794) and the
  terms the parser folds to a value (`Any`, `1e0`/`Inf`/`NaN`, `Empty`, #10802).
- **A spelling the parser normalizes** gets a flag on the internal node rather than a new node,
  the way `Stmt::If` keeps `is_unless` / `with_kind`: `Expr::Hash` carries a `HashSpelling`
  (`{…}` composer or `%(…)` contextualizer), which `convert` reads to pick
  `Circumfix::HashComposer` / `Contextualizer::Hash` and `lower` restores. Before it, both
  rendered as a `Block` that lowered to a closure — a wrong answer, not a refusal, which is
  why the `MUTSU_RAKUAST=1` failures that *run* deserve the same attention as the refusals.
- **The pattern for a parser expansion** (first used for `my ($a, @b) = …`): the parser splits
  the construct into a parse step that builds a *source-form record* and an expansion function
  that turns the record into the statements the compiler runs. The expansion opens with the
  record as a `Stmt::SourceForm` statement (`src/ast/signature_decl.rs`), which the compiler
  skips and the AST visitors do not walk (the expansion holds the same expressions). `convert`
  renders the RakuAST node from the record, never from the expansion; `lower` builds a record from
  the node and calls the *same* expansion function. This is §2.2's "move desugaring out of the
  parser" done in two steps: the expansion function is already the one desugaring site, and it
  moves into `lower` wholesale once the parser emits RakuAST (Stage 2). Code that inspected an
  expansion's shape to recognise the construct (`sink_warn::is_destructure_block`, the "all
  `VarDecl`" group-declaration checks) reads `ast::is_group_declaration` or skips the record.

- **Source positions (the §4 question, decided for lines).** A statement node carries the line it
  began on as a hidden `origin` field (`src/rakuast/origin.rs`): `convert` takes it from the
  `Stmt::SetLine` marker in front of the statement, `lower` puts the marker back, and the
  renderer, like Rakudo's `.raku`, shows no origin. A hand-built node has none and lowers
  without markers, as before. Before it, every line an error, a `warn` or a backtrace reported
  came out wrong under `MUTSU_RAKUAST=1`. Columns, and an origin on nodes below the statement
  level, wait for the first parser-emits-RakuAST slice; the representation (a field on the
  existing node, not a new struct member) keeps those two open.
- **A parser-side instrumentation the node cannot carry** is restored in `lower` by the parser's
  own function, never copied: the call-site markers the parser stamps on calls
  (`parser::callsite_line_arg`, `parser::stamp_call_site_markers`) come back from the statement's
  origin, and a routine the unit declares itself (`rakuast::declared_routines`) wins over the
  builtin statement of the same name (`sub take`, `sub say`, …) as the parser's scope made it
  win at parse time. A zero-argument call without the marker took a call path the parser's trees
  never reach — `EVAL 'return 5'` in such a sub escaped it.

### 6.1 S10: callable and destructuring signatures (2026-10-10)

Callable parameters preserve their whitespace sub-signature, including its
return type, and the colon signature constraint. Rakudo's `.raku` omits the
colon constraint, so `Parameter` keeps it as hidden `code-signature` metadata;
lowering restores `ParamDef::code_signature` without inventing a parser marker.
Hand-built whitespace sub-signatures populate the same callback metadata used
by NativeCall. A constrained pointy parameter retains its full `ParamDef`
instead of being reduced to a name-only `Lambda`.

Slurpy destructuring and named nested patterns use the existing `Signature`
and `Parameter` fields. Conversion follows a named alias chain to its actual
destructuring signature; lowering rebuilds the alias around that signature.
Anonymous named `:(...)` remains a single parameter rather than gaining an
alias level. Sigilless slurpies share the common parameter suffix handling,
including `where`, defaults, type captures and traits. Pointy parameters keep
their block defaults and container identity, and the block keeps its return
constraint. The `;;` dispatch boundary is retained as hidden `multi-invocant`
parameter metadata, another property Rakudo omits from `.raku`.
The executable contract
is `t/rakuast/rakuast-callable-and-slurpy-signatures.t`.

### 6.2 S10: written routine traits (2026-10-10)

`SourceForm::RoutineTraits` retains the traits in written order, before the
parser folds them into routine execution fields. It uses typed arguments,
including angle-word text, parenthesized expressions and export tags. Only a
spelling-keeping parse builds the record; ordinary parses retain their existing
execution tree. Named subs, methods, protos and anonymous routines share this
carrier. Lowering retains constructed trait lists through the same carrier.
The compiler skips this source form, and body-shape checks treat it as a marker
so a proto's `{*}` remains a dispatcher.

The source list replaces the converter's guessed flag order. Explicit DEFAULT
export arguments remain explicit, and `returns Positional of Int` produces one
parameterized return trait. Argument expressions are never evaluated while
recording or converting them. Proto custom-trait arguments use the existing
compiled declaration-expression chunks at registration; their trait target is
the live dispatcher, and its return constraint is retained. The cache format
is advanced for the typed source form and compiled proto arguments.

The executable contract is `t/rakuast/rakuast-routine-trait-order.t`, measured
against Rakudo for the read and EVAL directions. Existing execution fields
continue to define builtin-trait application; this slice does not introduce a
second trait evaluator.

### 6.3 S10: declaring signatures and role argument cohort (2026-10-10)

Declaring-signature parameters retain optional flags and default expressions;
literal postconstraints expose their value and the generated matcher over
`Term::Declaration`. Lowering uses the shared declaration expansion, including
its arity checks, lazy defaults and assigned-container result. A bare group's
default is retained as hidden `group-default` metadata because Rakudo omits it
from the constructor rendering. Nested declarator groups remain refused: their
parser representation currently flattens the leaves and must first retain the
actual structure.

A loose logical tail keeps the declaring expression as its left operand,
instead of discarding the declaration record while flattening the expansion.
The bare capture form of an invocant keeps hidden `capture-invocant` metadata;
Rakudo's renderer omits this flag, but lowering needs it to avoid consuming an
extra positional argument. Folded sigilless method invocants participate in
the existing typed name visitor.

Role application arguments use their parsed expressions and `BinaryForm` for
colonpairs, rather than splitting their source strings a second time. The
executable contract is `t/rakuast/rakuast-signature-type-cohort.t`.

The same cohort retains declarations and modified statements in expression
context, including nameless methods in a statement list. Semicolon-separated
array composer sections use the existing `Spelling` metadata: the compiler
still consumes the normalized execution tree, while conversion sees every
written section and lowering shares the parser's section builder. Empty sections
are `Statement::Empty` nodes and keep their ordinary `Any` array element.
Constructed composers lower every statement rather than only the first.
Expression-position proto subs return the registered code value, using the same
bytecode path as named sub declarations. The executable contract is
`t/rakuast/rakuast-expression-context-cohort.t`.

### 6.4 S10: labelled blocks and deferred callable names (2026-10-10)

Labelled bare blocks retain a spelling wrapper over the same labelled do-block
the compiler already executes. Conversion puts the label on the enclosing
`Statement::Expression`; lowering restores the labelled source block, rather
than synthesizing a loop. A label reference retains its parser-created Label
value as hidden `label-value` metadata on `Term::Name`, so source location and
identity survive the round trip. `Label.new` uses a named `name` field.
The abstract Statement model owns `labels`; inherited accessor defaults and
attribute/method introspection share that ownership rather than adding a local
field to Statement::Expression.
Constructed Term::Name label references resolve in an RAII-managed lexical
lowering scope; parsed label terms keep their original values. Constructed
labels have no source cursor (empty file and line zero).

Special callable method references use `Call::BlockMethod`, while `&?BLOCK`
and `&?ROUTINE` use the corresponding compiler-variable nodes. Ordinary and
hyper lowering share the existing dynamic method representation and bytecode.
Statement-expression constructors accept labels and modifier nodes too.

The shared `ast::bind_decl::expand` records callable binding intent independently
of scalar/aggregate runtime bookkeeping. Conversion recognizes this exact
expansion and emits `Initializer::Bind`; lowering uses the same expansion for
bound blocks, pointy blocks and existing callable values.

The parser's `ExportTermOrCall` remains deferred through hidden
`export-term-name` metadata on the fallback call. Lowering restores exactly
that compiler expression; conversion never runs an export hook to guess
whether the name will be a term. The executable contract is
`t/rakuast/rakuast-labelled-callable-cohort.t`.

Shadowable term keywords similarly retain their name and fallback value as
hidden metadata. Unknown names after a dynamic export hook remain value lookups;
known declared types and setting terms keep their ordinary node classes. Final
bare statement calls share the expression parser's deferred import decision.
EVAL routine preseeding includes caller-visible packages and lexical callable
aliases, excluding private short names from unrelated, previously loaded modules.

### 6.5 S10: control signatures (2026-10-10)

`ControlPointy` retains the written condition, signature and body of a pointy
`unless`, `while` or `until` before its shared executable expansion. The
compiler continues to consume ordinary conditional and loop bytecode; the
record is skipped like the other source forms. Loop destructuring happens on
each iteration, retaining `next` and `last` in the loop's own scope. `until`
parameters receive the original condition value before its truth is negated.
Labelled while/until clauses use this same parser and retain the label on
their source record and executable loop.

`orwith` uses the same pointy parameter parser and body expansion as `with`,
retaining type constraints, sigilless names and destructuring. Its condition
is saved once; the body and any trailing `else` consume that saved value.
Pointy `else` blocks retain the existing signature source record, and lowering
installs the last conditional's binding even when its then block has no
explicit signature. That temporary never becomes a visible then parameter.

The conditional and while/until constructors accept their condition and block
fields, inherited labels and applicable continuation fields. Initializer
parentheses are restored at the assignment boundary so multi-match result
itemization agrees with the parser's tree. The read, constructed-tree and
semantic contracts are pinned in
`t/rakuast/rakuast-control-signature-cohort.t`.

Before copying a control source record, a typed visitor counts its executable
and recorded subtrees, with a 16,384-node and 128-level limit. Nested records
cannot grow exponentially until allocation fails: parsing and constructed-tree
lowering return a catchable resource error before the next oversized copy.
The visitor also bounds parameter and regex traversal. Parser unit tests pin
both the copy budget and its propagation through the conditional and loop
families. With-family expansion copies a body only for an explicit signature,
and moves that copy into the record rather than cloning it again.

When EVAL conversion refuses a unit, its existing static source validation
supplies any typed CHECK-time diagnostic before the refusal is returned.
Valid converted units are checked after lowering, when imports and deferred
names have been resolved. No refused unit executes the original parser tree.
`t/rakuast/rakuast-eval-source-diagnostics.t` pins these boundaries.
Caller type and term names are captured before parsing or static module probes
can reset import tables. Runtime import aliases also supply caller terms,
so cold and cached module loads use the same names.
Mainline diagnostic conversion remains a separate residual path.
An ordinary-frontend gap for lowercase constants imported inside EVAL is
tracked separately in #12574.

### 6.6 S10: declaration modifiers and named declarator lists (2026-10-11)

`DeclarationModifier` records the written declaration, condition and keyword
before initializer hoisting. Conversion reads that record instead of guessing
the original declaration from assignments to staging locals. `if`, `unless`,
`with` and `without` share their declaration expansions between the parser and
RakuAST lowering. The declaration remains unconditional, while assignment or
binding runs only when the modifier selects it. Topical modifiers support
declarator lists as well as individual variables; a list binding retains its
complete staging, arity checks and aliases. Compile-time constants and bare
declarations retain their written modifiers without changing execution.

Named declarator lists now retain `SignatureDecl` too. The ordinary parser and
constructed signature lowering both use the same named-part expansion, including
the empty-array default for an absent named array. Named declarations also use
the common loose-word-logical and statement-modifier parsing boundaries.
Assignment to a declaring signature uses positional STORE semantics even when
its parameters have names. Binding uses the named part when appropriate and
yields the declaring Signature value, materialized through the same helpers as
signature literals. Positional and named binding results share that construction
and retain parameter names, optionality, slurpy flags and traits.

The source-copy visitor counts declaration records, their original declarations
and conditions before copying them, with the same node and depth budgets as
control signatures. Signature expansion also validates the parameter and
expression budget before materializing its result; parser and lowering paths
propagate the resource error without replacing the expansion. Source-record
and signature traversal belongs to the exhaustive typed AST visitor. Its
source-record hook defaults to skipping compiler-inert duplicates, while the
copy budget opts into walking them. The compiler discards net-value statements inside a
statement-position synthetic block, as it does in other discarded statement
sequences; an initializer's topicalizer can no longer leave a value underneath
the enclosing unit's real result. Expression and tail-position synthetic blocks
continue through their existing value-producing compilation paths.

All nine single-expression conditional and loop modifier classes expose their
positional constructors. Read, constructed-tree and execution behavior is pinned
in `t/rakuast/rakuast-declaration-modifier-cohort.t`, measured against Rakudo.
The large modifier parser is separated into statement-chain parsing, individual
modifiers and for-modifier signature handling. Shared declaration expansion,
synthetic-statement compilation and positional constructors have their own
modules. Richer declaration forms that do not yet support initializer hoisting
retain the ordinary modifier path; nested declaration groups remain a separate
source-representation remainder.

### 6.7 S10: regex source retention cohort (2026-10-11)

The residual regex cohort preserves numbered aliases, nested assertion prefixes,
Unicode quotes and interpolating qq atoms in the shared source tree. Separated
quantifier reconstruction retains the observable whitespace placement around
atoms and separators, including the alias body's measured wrapper shape.
Source delimiter helpers are shared with the parser and matcher; no embedded
code runs while a tree is built. ADR-0088 section 64 records the contract, and
`t/rakuast/rakuast-regex-source-cohort.t` covers both directions and execution.

### 6.8 S10: type composition and attribute shapes (2026-10-11)

`HasDecl` retains a hash key type independently of its optional written value
type. The compiler's existing attribute descriptor combines the two once;
RakuAST converts the key to `shape` and the written value type to `type`, and
lowering restores both fields without inventing an explicit `Any`. The typed
visitor visits both type names, and compile-time type-argument validation
checks both. This source-layout change advances the precompilation schema to 25.

Definedness, coercion and parameterized types share bounded recursive building
and lowering. Every base and argument edge consumes the same 128-level budget.
Simple and qualified names use the parser's complete identifier grammar. Loop
setup now reuses ordinary statement conversion and unwraps the expression,
preserving typed declarations and explicit initializers rather than inferring
one from a non-Nil value. `t/rakuast/rakuast-type-shape-cohort.t` pins the measured
structure, constructed type nodes and execution through the existing compiler.
The shared `VarDeclaration::Simple` constructor retains scope, type, shape,
traits, initializer and where fields, including attribute twigils. Constructed
declarations therefore apply their supplied constraints as source declarations do.
The attribute write-direction test also pins key validation before mutation:
named, accessor and computed object-hash stores share key preparation, while
QuantHash retains its numeric-allomorph rules.

Untyped loop header declarations record their explicit initializer through the
same source marker as ordinary declarations. Numeric return constraints use
literal nodes, separately from strict type identifiers, and lower back to pinned
return values. The frontend ratchet exposed both distinctions and the cohort
pins their structure and execution.

### 6.9 S10: indexed binding source forms (2026-10-11)

Indexed binding retains whether its index came from semicolon-separated
dimensions, and conversion and lowering preserve Rakudo's `SemiList` shape:
comma-separated slice operands stay inside one list-infix statement, while
semicolon dimensions remain separate expressions. The compiler routes a
semicolon bind through the existing deep lvalue operation so the terminal
element is rebound in place and aliases the RHS scalar. This also fixes the
ordinary execution path, which previously flattened dimensions into a comma
slice and wrote to the wrong row. `t/rakuast/rakuast-index-bind.t` pins both
source shapes and round-trip execution; `t/routines/dispatch/indexed-multidim-bind.t`
pins ordinary leaf selection and aliasing.

### 6.10 S10: callable and starred list assignment (2026-10-12)

Callable lvalue assignment retains the complete target expression through the
RakuAST call form, including indexed values, method objects and adverb-bearing
subscripts. Parenthesized declarations retain their `Circumfix::Parentheses`
source node and lower through the parser's callable-lvalue assignment path.
The one-array/one-Whatever assignment form reconstructs its written list
target from the parser's RHS offset helper, preserving the skipped RHS prefix
and the target position. `t/rakuast/rakuast-callable-lvalue.t` pins declaration
and star-target structure; the existing callable-lvalue and star-list tests
exercise execution in round-trip mode.

### 6.11 S10: indirect method lvalues and quoted dotty calls (2026-10-12)

Quoted and interpolated method names on assignment targets lower back through
the parser's dynamic method-lvalue path, preserving the source call and its
writeback rules. Dotty assignment accepts a quoted method call as its right
side, including method arguments, and lowers it through the shared `.=`
expansion. `t/rakuast/rakuast-callable-lvalue.t` pins both node shapes; the
method-lvalue and dotty RMW suites pass in round-trip mode.

### 6.12 S10: supply statement source forms (2026-10-12)

`supply STATEMENT` and `supply whenever ...` now retain the written body in
the same `SupplyBlock` source record as `supply { ... }`. Conversion therefore
renders the source-level `StatementPrefix::Supply` rather than exposing the
generated on-demand emitter lambda, and lowering reuses the shared supply
expansion. The existing seven first-refusal supply tests pass in RakuAST mode;
`t/rakuast/rakuast-supply-block.t` pins both node shape and execution. The
frontend ratchet now passes 6938 of 7259 files, with 6941 entries including
retained prior results.

### 6.13 S10: unresolved qualified names (2026-10-12)

An unresolved qualified bareword (`A::B`) is a valid `Call::Name`, even when
it is the operand of a method call. Conversion now preserves that measured
node shape; lowering routes it through the parser's ordinary named call path,
so lookup errors remain catchable at evaluation time. The focused round-trip
test covers both the method-call shape and a `try` around a missing qualified
type. The frontend ratchet passes 6952 of 7268 files.

### 6.14 S10: object-hash composer (2026-10-12)

The parser's `:{ ... }` spelling uses the same
`RakuAST::Circumfix::HashComposer` as `{ ... }`, with `object-hash => True`.
Conversion now preserves the expression and flag, including empty and
multi-pair composers; lowering restores the existing `__object_hash` call so
Mu-keyed key identity and current execution semantics remain shared. The
constructor and accessor expose the optional flag, whose absent value is
`False`. `t/rakuast/rakuast-object-hash.t` covers read and constructed trees,
ordinary-hash distinction, and evaluation. The frontend ratchet passes 6960
of 7274 files and lists 6973 entries.

### 6.15 S10: bare `done` calls (2026-10-12)

Rakudo renders `done` without parentheses as
`Call::Name::WithoutParentheses`. Conversion recognizes this core call, and
lowering restores the parser's `Expr::BareWord("done")` marker so the compiler
retains its supply/react completion behavior in expression position.
`t/rakuast/rakuast-bareword-resolution.t` pins the node shape, and
`t/concurrency/supply/supply-done-expression.t` passes in RakuAST mode. The
frontend ratchet adds 36 files on the current main tree.

### 6.16 S10: triple-pipe regex alternation (2026-10-12)

The regex source-tree parser consumes `|||` as one sequential-alternation
operator, matching Rakudo's `Regex::SequentialAlternation` shape. It rejects
pipe runs longer than three instead of inventing empty branches. The existing
matcher still executes the source pattern. `t/rakuast/rakuast-regex-source-
cohort.t` pins the node and round-trip match; `t/regex/null-regex.t` exercises
the EVAL path in RakuAST mode.

### 6.17 S10: attribute handles source terms (2026-10-12)

Attribute `handles` clauses retain their written term through conversion,
including parenthesized rename pairs, mixed dynamic lists, and regex terms.
Bare identifiers inside a handles list denote delegated method names and are
converted as string terms; identifiers inside a slipped expression keep their
ordinary expression meaning. Lowering derives delegation specs from the term
and uses the existing attribute composition path. The focused RakuAST test
covers term structure and evaluation; all five existing first-refusal files
for handles spelling pass in round-trip mode. After refreshing the ratchet on
the current main tree, it lists 7,041 files, 18 more than the branch base.

### 6.18 S10: temporary method and attribute lvalues (2026-10-12)

`temp $.attr = value` and `temp $obj.attr OP= value` are rendered as the
ordinary prefix and assignment nodes. The parser retains whether the original
invocant was an attribute, since `temp $.attr` and `temp self.attr` have
different RakuAST operands. Compound assignments recover their operator and
right-hand side from the parser's assignment identity marker, then lower back
through the existing temporary save/restore statement. The focused test pins
public/private attribute and method-call shapes and checks restoration for
plain and compound assignments; the three first-refusal files pass in RakuAST
mode. The frontend ratchet adds 12 files, bringing it to 7,053 passing files.

### 6.19 S10: imported bareword calls (2026-10-12)

When the parser has resolved a bare identifier to an imported function, it
retains that source classification through RakuAST conversion. The converter
renders it as `Call::Name::WithoutParentheses`, matching Rakudo; lowering
returns to the existing bare-call path. Unresolved names remain refusals, so
the conversion does not turn a compile-time undeclared-name error into a
runtime lookup. The focused test pins the node and EVAL result, and
`t/collections/subscript/brace-subscript-postfix.t` now passes in round-trip
mode with the imported call followed by a subscript. The refreshed frontend
ratchet lists 7,056 files, three more than after #12992 including the current
main coverage refresh.

### 6.20 S10: attribute statement modifiers (2026-10-13)

An attribute declaration with a postfix condition modifier retains the
declaration, condition and modifier kind as a source-form record. The parser
and RakuAST lowerer use the same expansion: the attribute is installed
unconditionally while the modifier's condition still runs in the class body.
Conversion renders the source declaration with its `condition-modifier`
instead of inspecting the generated modifier statements. The focused RakuAST
test pins `when` and `if` structure, the required-attribute behavior, and one
condition evaluation during class composition.

### 6.21 S10: grouped and colon-arglist attributes (2026-10-13)

Parenthesized `has Type ($.a, $.b)` declarations convert to Rakudo's single
`VarDeclaration::Signature`, while hidden model metadata retains each
attribute's traits and defaults for lowering through the parser's shared
class-body expansion. For `has $.a: has $.b`, conversion displays the
initiating declaration as Rakudo does and retains declarations from its colon
argument list as hidden model metadata. The RakuAST round-trip tests cover
typed groups, `is rw`, typed defaults, nested colon declarations, and chained
colon lists. The refreshed frontend ratchet passes 7,101 of 7,389 files.

### 6.22 S10: role name adverbs (2026-10-13)

Role declarations with `:ver`, `:auth` and `:api` now pass through the shared
package-header conversion instead of being refused as parser-generated
synthetic blocks. Conversion keeps the adverbs as colonpairs on the role name
and retains a following parameterization signature. Lowering restores the
parser's metadata setters around the role declaration, which keeps role body
lexicals in scope for its methods. The focused RakuAST test checks the measured
name and signature shape, round-trip composition, and a role body lexical. The
frontend ratchet now passes 7,105 of 7,393 files.

### 6.23 S10: exported declarations and imported type names (2026-10-13)

The parser keeps `is export` on a `constant` or an `our` variable as the
declaration's `is_export` flag and tags, not as a custom trait, and the
converter used to drop both: under `MUTSU_RAKUAST=1` an exported constant
nested in a routine body was not exported at all. Conversion now renders the
flag as rakudo's `Trait::Is(export)` (with the tags as its colonpair
argument, measured on 2026.09), and lowering reads that trait back into the
flag, so both directions go through the parser's own export registration.
Like `is dynamic`, the flag has lost its position among other traits, so an
exported declaration with further traits stays the boundary.

A bareword naming an imported type resolves only if the importer's module
scan (`decl_scan.rs`) lists it. The scan now lists what rakudo imports and
it had missed: a lexical `my class X is export` nested in a routine or
block, a `class GLOBAL::X` (an absolute name), and the short name of an
exported qualified type (`class A::B::C is export` imports `C`). These were
the `BareWord` first refusals of the `t/modules/` import tests; a name the
importer cannot see in rakudo either (an undeclared name under `try`) stays
a refusal.

### 6.24 S10: immediately called routine declarations (2026-10-13)

`sub foo($n) { … }(21)` declares `foo` and calls it in place. The parser
expands it into the declaration followed by a call of the declared name;
rakudo's tree is an `ApplyPostfix` whose operand is the `Sub` and whose
postfix is a `Call::Term` (measured on 2026.09). `ast::immediate_call::expand`
is the one builder: a spelling-keeping parse opens the expansion with a
`SourceForm::ImmediateCall` record, which the converter reads to render the
source form, and lowering hands a measured node back to the same function, so
the routine stays declared in the enclosing scope. Under `do` the node is
the statement of a `StatementPrefix::Do`. A routine declaration with a
statement modifier (`sub f { } given …`) is a separate parser expansion and
stays a refusal.

### 6.25 S10: compound-assignment declarations (2026-10-13)

`my $x OP= EXPR` parses to the declaration (with its default value) and one
assignment of the operator applied to it; rakudo's tree is an `ApplyInfix`
whose `left` is the `VarDeclaration` and whose infix is `MetaInfix::Assign`
(measured on 2026.09). `ast::decl_compound_assign::expand` builds the
expansion; when the written operator is known (a spelling-keeping parse of a
built-in operator, or the lowering) it opens with a
`SourceForm::DeclarationCompoundAssign` record and the assignment keeps the
operator and right-hand side in its `Expr::CompoundAssign` marker, which the
compiler already sees through. A postfix `for` hoists the declaration out of
the loop (`split_for_modifier`, shared by the parser and the lowering), so the
variable is declared once rather than reset per iteration; the converter
renders that loop as the assignment statement with its `loop-modifier` and the
declaration as the assignment's left operand. `Z=` and user-defined compound
operators keep their own expansions and stay refusals.

### 6.26 S10: type-smiley pragmas (2026-10-13)

`use variables :D` (and `attributes`, `parameters`, `invocant`) is a
`RakuAST::Pragma` whose `argument` is a `ColonPair::True` of the smiley
(measured on 2026.09). The parser keeps the smiley as a `":D"` string
argument; only the colonpair spelling produces it (a written string argument
is a parse error), so the string is the smiley's own record rather than a
lost distinction. Conversion renders the colonpair, and lowering turns the
colonpair, which the generic `use` lowering would read as an import tag,
back into that argument.

### 6.27 S10: EXPORTHOW::DECLARE declarators (2026-10-13)

A `use`d module can register a class-like keyword through
`EXPORTHOW::DECLARE` (`monitor Name { }`, `shouter Name { }`). Measured on
rakudo 2026.09, the declaration is the ordinary `RakuAST::Class` (or
`RakuAST::Role`, for a role HOW) with a `parsed-declarator` field holding the
keyword, after `scope` and before `name`. The resolved HOW is an attribute of
the node but not part of its rendered tree, and `.parsed-declarator` answers
the built-in keyword (`class`, `role`, `grammar`) otherwise. The parser's
`DECLARE_HOW_TRAIT` marker already holds that keyword, so it is the field's
record: conversion renders it (`rakuast::declare_how`), lowering hands it back
through the package head, and registration resolves the HOW from the keyword
exactly as for a parsed declaration. Nothing has to encode the metaobject.

### 6.28 S10: symbolic subrule calls in regexes (2026-10-13)

`<::(EXPR)>` calls the rule whose name `EXPR` computes when the cursor reaches
it. Measured on rakudo 2026.09, it is a `Regex::Assertion::Named` whose name
is `Name(Part::Empty, Part::Expression(EXPR))`, with `capturing => True`
unless written `<.::(EXPR)>`; `<q=::(EXPR)>` wraps it in
`Regex::Assertion::Alias`. The regex tree had no node for it, so a whole regex
holding one lost its tree. `RegexNode::SymbolicSubrule` keeps the parsed
expression and its written source; execution stays on the runtime parser path
(the matcher resolves the name from the regex text, as before), and lowering
rebuilds the source with `expression_source`, which now also renders an
interpolated string.

### 6.29 S10: named `anon` declarations (2026-10-13)

`anon sub g { }`, `anon class Foo { }`, `anon role R { }` and
`anon grammar G { }` build a routine or type that keeps its name but is not
installed under it. Measured on rakudo 2026.09, each is the ordinary `Sub` /
`Class` / `Role` / `Grammar` with `scope => "anon"` before the name (a
nameless `anon class { }` has the scope and no name). The parser's
`ANON_DECL_TRAIT` marker is that scope: conversion renders it, and lowering
marks the declaration again (`mark_anon_package_stmt`, which also gives a
package's registry name its site suffix). A statement-level `anon sub` lowers
back to the parser's expression statement, which declares nothing.

A statement-level `anon class C { }` was a second mechanism: it registered
`C` and then removed it again with a `__MUTSU_UNREGISTER_CLASS__` call. It
now parses to the expression form's marked declaration, so there is one
mechanism; the builtin and the compiler's special case for its block are gone.

### 6.30 S10: package-qualified variable declarations (2026-10-13)

`my $pkg::x` declares a variable in the stub package `pkg`. Measured on
rakudo 2026.09, the declaration's `desigilname` is
`Name.from-identifier-parts("pkg", "x")`, which the converter already built;
lowering now reads a multi-part desigilname back as the parser's `::`-joined
name. The declaration also makes `pkg` a type at parse time, so the unit's
name scan lists the stub packages of a qualified variable, and `OUR::A` /
`MY::A` over a name that resolves is a `Term::Name` of the qualified name,
whether `A` is a type or a term (it was a `Call::Name`, which lowered to a
sub lookup).

### 6.31 S10: regex boundary assertions and other tree gaps (2026-10-13)

Several regex spellings had no regex-tree node, so a regex holding one lost
its whole tree and refused to round-trip. Measured on rakudo 2026.09:
`<|b>`, `<|c>` and `<|g>` are `Assertion::Pass` (a grapheme or codepoint
boundary holds at every position of an NFG string), and `<?|w>` / `<!|w>` /
`<!|b>` are an `Assertion::Lookahead` over `wb` / `Pass`; the tree models
them so, and spells a lookahead over `Pass` back as `<?>` / `<!>`. An inline
adverb before an atom (`:i<[ab]>`) is the modifier followed by the atom,
`*:?` / `*:!` are the frugal / greedy quantifier, `@$ref` is an
`Interpolation` over `Contextualizer::List(Var::Lexical)` (a new
`RegexExtension::ContextualizedVariable`), `$( a; b )` may hold several
statements, `<rx={$r}>` is an `Assertion::Alias` over an
`InterpolatedBlock`, a lookaround may hold any RakuAST-only construct (a
back-reference), and a regex literal argument of a subrule call
(`<v(/\d+/)>`) is spelled back from its tree. Execution keeps the regex text.

Two runtime-parser bugs surfaced on the way and are fixed in the matcher's
`<?before …>` body scanner: a bracketed escape inside a class
(`<-[\x[22]]>`) and whitespace between the parts of a compound class
(`<[0..9] + [x]>`) closed the assertion early; and an inline adverb written
right after an atom (`/x:i<[ab]>/`) was read as that atom's `:` backtrack
control.

### 6.32 S10: regex placeholders, scalar callees and package variables (2026-10-13)

Measured on rakudo 2026.09: `/$^a/` is a `Regex::Interpolation` whose `var`
is a `VarDeclaration::Placeholder::Positional` (a new
`RegexExtension::Placeholder`; the enclosing block still finds the
placeholder by scanning the lowered regex text, as for a parsed one);
`<&$tok(1)>` is an `Assertion::Callable` whose callee is
`Contextualizer::Item(Var::Lexical("$tok"))` (the tree's `Callable` keeps
the name with its `$`); `<@Pkg::list>` is an `Assertion::InterpolatedVar`
over a `Var::Package`. A `:my token T { … }` statement whose block ends its
line needs no `;` and takes no `WithWhitespace` wrapper, and the quotes in
the character class of a regex nested in `$( … )` no longer open a string
in the contextualizer's scanner.
### 6.33 S10: a declaration followed by a postfix (2026-10-13)

`my @a.[2] = 42`, `my %h.<k> = 1`, `my @a.push(1)` and `my Int $x.say`
declare the variable and apply the postfix to it. Measured on rakudo 2026.09,
each is one `ApplyPostfix` whose operand is the `VarDeclaration::Simple` --
the node `(my @a)[2] = 42` builds too, since rakudo drops the parentheses of
a postfix's operand. The two compute the same thing, so the converter now
puts the declaration back under a postfix of the parser's declaration tail
(`decl_tail`), and lowering keeps reading that node the parenthesized way.
The parser's tail read the declared array or hash as `Var("@a")` rather than
the `ArrayVar` / `HashVar` a written `@a` is; it now builds the same node.

### 6.34 S10: enum base types, colonpair words and computed bodies (2026-10-13)

An enum's body is one term rakudo evaluates at compile time, and
`Type::Enum.term` keeps it as written. Measured on rakudo 2026.09: a base
type (`our Str enum E ...`) is the `of` field (a `Type::Simple`, between
`name` and `traits`); a colonpair word of a `«…»` body is a `QuoteWordsAtom`
segment over a `ColonPair::Value` between the literal words; a parenthesized
body the parser cannot decompose (`enum E (:20A, slip <B C>)`) is the
`Circumfix::Parentheses` term itself. The converter renders the three from
the parser's variants (`src/rakuast/enum_term.rs`), and lowering decomposes
a parenthesized term with the parser's own `enum_variants_from_body`, so a
round-tripped enum takes the static or the computed path the parsed one took.
The value names a computed body introduces (its static pairs and the words
of its `slip` entries) come from one parser helper, `enum_value_names`, which
the converter's declared-name table and the module export scan share; an
`our` enum's values are also declared under the enclosing packages
(`module M { enum E <a> }; M::a`).

### 6.35 S10: hyper prefix operators (2026-10-13)

`-« @a`, `++<< @a` and the other hyper prefixes are, on rakudo 2026.09, an
`ApplyPrefix` whose `prefix` is a `MetaPrefix::Hyper` over the plain
`Prefix`. The parser spells the operation as the call
`__mutsu_hyper_prefix("-", arg)` the runtime executes; that call is the
lowered form, so the converter reads it as the node and lowering builds it
back (`src/rakuast/hyper_prefix.rs`), the way the atomic operators map to
`ast::atomic_op`'s calls.

### 6.36 S10: package-qualified constants of a loaded module (2026-10-13)

A `use`d module's `our constant NAME` is installed in its package, so
rakudo 2026.09 renders `Pkg::NAME` in the importer as a `Term::Name`; only a
name it cannot resolve is a `Call::Name`. The module export scan collected
constants by their bare name only, so the converter rendered the qualified
spelling as a call and the round trip looked up `&NAME`. The scan now also
collects an `our` constant under its package path (a `my` constant stays
lexical to the module), the way it already did for an `our` enum's values.

### 6.37 S10: `$.name OP= EXPR` as a statement (2026-10-13)

The statement parser spells `$.n += 100` as a method compound assignment on
`__ANON_STATE__`, the bare-`$` carrier the VM resolves to `self`; the
expression parser spells `($.n += 100)` over `Var(".n")`. Rakudo 2026.09
builds one node for both: `ApplyInfix(left => Var::Attribute::Public("$.n"),
infix => MetaInfix::Assign(Infix("+")))`. The converter now reads the
statement spelling as that node, and lowering keeps producing the expression
spelling, which computes the same thing (an `is rw` accessor assigns; a
read-only one assigns into a throwaway container).

### 6.38 S10: loop controls with a value or a slip (2026-10-13)

Rakudo 2026.09 renders every loop control as a bare
`Call::Name::WithoutParentheses` over its argument, whether that is a label
(`next L`, a `Term::Name`), a v6.e value (`last $_ * 10`, `next Empty`) or a
slip (`next |$c`). Statement lowering only accepted a label and refused the
rest; a non-label argument now lowers to the expression form the parser keeps
for it. Two parser rules are shared rather than restated: a bare word is a
label when one of that name is in scope or by the parser's own
`is_loop_control_label` (any word before v6.e, a declared or all-caps one
from v6.e), and a slipped argument stays the call form in
`loop_control_expr`, as `loop_control_call_form` parses it.

### 6.39 S10: names a loaded module or a package alias makes known (2026-10-13)

Rakudo resolves a name while parsing, so its node says what the name is. The
converter learns the names of a loaded module from the parse-time module
scan, which missed five kinds; each rendered as a `Call::Name` (the round
trip then looked up `&NAME`) or was refused. Measured on rakudo 2026.09, all
are terms or packages:

- an `our` enum's values qualified by the enum's own name, from any module
  loaded transitively (`enum DepsShort::LC <New>` → `DepsShort::LC::New`);
- a qualified enum or role exported with `is export`, under its short name,
  the way a class already was (`my enum M::Endian is export` makes
  `Endian::Vax` a value, `unit role M::Greeter is export` makes `Greeter` a type);
- the enum values an `import Module :TAG` selects (only `use` replayed the
  tagged values before; an `import` now replays the scan with its tags);
- a module's `EXPORT`, `EXPORT::DEFAULT` and `EXPORT::ALL` packages, which
  an `is export` routine creates;
- a sigilless value bound into an export stash at BEGIN time
  (`EXPORT::special::<Answer> := 42`), which the scan now records like an
  exported constant.

Within one unit, a `constant` bound to a package the unit declares is that
package (`constant E = Outer::Inner`): the converter renders `E` as a
`Type::Simple` and copies every name declared under the target to the alias
(`E::Status::Started`), in `src/rakuast/bareword/package_alias.rs`.

### 6.40 S10: `.=` with a term as the method (2026-10-13)

Rakudo 2026.09 renders the method of a `.=` whose method is not a name as
the term itself: `$x .= &f` and `$x .= $m` are `ApplyDottyInfix(right =>
Call::TermAsMethod(callee => …))`, even for `&f` (after a plain `.`, `$o.&f`
is a `Call::NameAsMethod`), and an interpolated quoted name (`$x .=
"{…}"(42)`) is a `Call::QuotedMethod`. A declaration has the same call in
its `Initializer::CallAssign` (`my %h .= &f`, accessor `postfixish`). Lowering
takes the call over a placeholder invocant and gives each target the parser's
`.=` expansion reaches its own copy, so the static and the dynamic forms share
one path.

The declaration's source form (`ast::method_assign_decl::MethodAssignDecl`)
now also keeps a term as the method (`callee`), a trailing postfix chain
(`chain`) and a shaped array's dimensions (`shape`). The chain applies to the
new value and is sunk (`my $o .= $m.uc`); rakudo makes the declaration the
innermost operand of the chain's `ApplyPostfix`, and lowering rebuilds the
form with the chain over the declared variable. The shape is source-only: the
parser's call already builds the container (`my @c[2;2] .= new(:shape(2,2),
…)`), so the expansion is unchanged.

### 6.41 S10: interpolating `words` quotes and colonpair word quotes (2026-10-13)

An interpolating `words` quote (`qqw/a $x b/`, `qq:w/$foo $bar/`) is, on
rakudo 2026.09, a `QuotedString` with `processors => ("words",)` (`<words
val>` with `:v`) over the string's interpolation segments: the words are only
known once the string is built. The parser spells it as the call
`__mutsu_words_atom(STRING)` (`__mutsu_quotewords_atom` with allomorphs) the
runtime splits; that call is the lowered form, so the converter reads it as
the node and lowering rebuilds it over the interpolation
(`src/rakuast/word_quote.rs`).

A colonpair whose value is a `«…»` / `<<…>>` word quote (`:args«green
yellow»`) is a `ColonPair::Value` over the same `QuotedString(processors =>
<quotewords val>)` a bare `«…»` term is. The colonpair parser built the word
list without the term's spelling record; it now parses the value with the
term parser (`french_quote_term` / `double_angle_term`), which evaluates to the
same list. A `<<…>>` that nests its own opener (`<< a <<b>> c >>`) is still
rendered as one literal run; rakudo nests a `QuotedString` there, and that
spelling stays open.

### 6.42 S10: package-qualified symbolic lookups (2026-10-13)

`MY::(q[$x])`, `CALLERS::('$_')` and `GLOBAL::('Int')` look a symbol up
under a written package. Rakudo 2026.09 keeps the package and the key apart:
`Name(Part::Simple("MY"), Part::Expression(EXPR))`, under a `Term::Name`
when the first segment is a lexical pseudo-package (`MY`, `OUR`, `CORE`,
`SETTING`, `UNIT`, `OUTER(S)`, `CALLER(S)`, `DYNAMIC`, `LEXICAL`, `CLIENT`)
and under a `Call::Name` otherwise (`GLOBAL`, `PROCESS`, `EXPORT`, a user
package). The parser concatenates `"PKG::" ~ EXPR` into the symbolic
dereference the runtime performs, which a converter cannot tell from a
written `::("MY::" ~ $k)`, so the lookup records the two parts as a spelling
(`Spelling::PackageIndirect`) and lowering builds the concatenation back
(`src/rakuast/symbolic_deref.rs`). A lookup with further `::(…)` / `::name`
segments after the first keeps the old boundary.

### 6.43 S10: compound-assignment substitutions (2026-10-13)

`s[PAT] OP= EXPR` replaces each match by `$/ OP EXPR`. Rakudo 2026.09 renders
it as `Substitution(infix => MetaInfix::Assign(Infix(OP)), replacement =>
EXPR)`, the sibling of `s[PAT] = EXPR`'s `infix => Assignment`, and answers
the Match (a List of them under `:g`). The parser used to desugar it to
`$_ = $_.subst(rx, { $/ OP EXPR }, :g)`: no tree to show, only `:g` of its
adverbs honoured, and a Str for its value. It is now an `Expr::Subst` whose
`replacement_thunk` is an `ast::SubstThunk`: the per-match expression
(`$/ OP EXPR`, built by `parser::subst_compound_thunk` for both the parser and
the lowering) with the written `OP` beside it (`SubstThunk::infix`), so the
converter renders the written EXPR (`parser::subst_compound_rhs`) and the
operator without reconstructing either. The `=` form is unchanged.

### 6.44 S10: `handles` on a method (2026-10-13)

`method NAME handles TERM { … }` makes the method a delegation source.
Rakudo 2026.09 renders `Method(traits => (Trait::Handles(TERM),))`, the
written term (`QuotedString(<words val>)` for `<a b>`, `Term::Whatever` for
`*`) in written order among the other traits. `Stmt::MethodDecl` keeps only
the folded `handles: Vec<HandleSpec>`, and a field beside it would touch every
`MethodDecl` constructor; instead the term rides the routine source-trait
record the parser already keeps in `.AST` mode
(`ast::routine_trait::RoutineTrait::Handles`, attached to the body like
`is`/`returns`/`of`). The converter renders it, and lowering rebuilds the specs
from the retained terms with `parser::handle_specs_from_term`, the routine the
attribute form uses. A clause whose spelling the parser does not rebuild specs
from (`parser::handles_clause_term` answers `None`) records an `Unsupported`
trait, so it stays the boundary.

### 6.45 S10: constant colonpair statements and numeric colonpairs (2026-10-13)

A colonpair written as a statement (`:a(7);`, the body of `"{:a}"`, the
pairs of `@{:a, :b}`) is folded by the parser to `LiteralSrc(Pair, ":a(7)")`,
so a sink warning can echo the written form. Rakudo 2026.09 shows the written
`ColonPair::Value` / `True` / `False` / `Number`. The source text is exactly
the colonpair, so the converter parses it again with the parser's own
colonpair routine (`parser::colonpair_source_expr`) and renders that; lowering
gives the ordinary colonpair `Binary`, so a round-tripped bare colonpair
statement words its sink warning like a fatarrow. `:7a` had no form of its
own and rendered as `FatArrow`; it is now `BinaryForm::ColonPairNumber`,
rendered as `ColonPair::Number(key, value)` and lowered back to the same form.

### 6.46 S10: adverbs on a user-declared infix (2026-10-13)

`1 foo 3 :round` passes `:round` to `infix:<foo>`. The parser appends the
trailing colonpairs to the `Expr::InfixFunc` operands
(`attach_trailing_adverbs`); rakudo 2026.09 renders them as the
`ApplyInfix`'s `colonpairs`. The converter splits the trailing adverbs off
the operands (a two-operand application only; a list-associative chain with
adverbs stays the boundary), and lowering appends them to the `InfixFunc`
call again. An application with adverbs is always a routine call, also for a
core operator the unit declares (`infix:</>`), whose plain application keeps
the native layer; one whose operator the unit does not declare is refused.

### 6.47 S10: user operators named with a `__` prefix (2026-10-13)

The converter refused an `Expr::InfixFunc` whose name starts with `__`,
assuming the parser used such names for its own helpers. No parser path
builds one: the only non-operator `InfixFunc` names are the class trait `is`
and `§`. A user operator may be named `__` (`infix:<__>`, exported from an
`EXPORT` sub in Understitch's style); rakudo 2026.09 renders it as an
ordinary `ApplyInfix(Infix("__"))`, and `[__]` as a `Term::Reduce`. The
prefix test is gone.

### 6.48 S10: names of the scope that runs `EVAL` or `Str.AST` (2026-10-13)

An `EVAL` string's bare names are classified against the names its calling
scope declares (`CallerNames`). The scan of constant markers read only the
current frame's env tier, so an outer constant was lost from a block whose
frame does not flatten its outer scope (`constant b = 1; { EVAL 'b' }; sub f
{ }`). It now walks every enclosing scope (`Env::visible_keys_where`), and,
for code running in GLOBAL, also takes the `our`-scoped constants of blocks
that have already ended (`{ constant c = 1 }; EVAL 'c'`), which `our_vars`
keeps under their `\name` spelling.

`Str.AST` installs the same names. Rakudo resolves the source's bare names
against the scope that runs `.AST` and renders a caller's constant *and* a
caller's class as `Term::Name` (only the source's own and the setting's types
are `Type::Simple`), so `.AST` passes all of them as terms.

### 6.49 S10: a sigilless term bound to a declarator (2026-10-13)

`my \foo = my $x = 3` and `my \foo = my $ = -3` bind `foo` to the new
Scalar. The parser splits them into the inner declaration (an anonymous `$`
renamed `anon-sigilless-src-N`, so two of them in a scope stay apart) followed
by the sigilless bind to its variable. Rakudo 2026.09 renders the inner
declaration itself as the initializer:
`VarDeclaration::Term(initializer => Initializer::Assign(VarDeclaration::Simple
| VarDeclaration::Anonymous))`. `sigilless_decl::declaration` now recognises
the pair (`declarator`), the converter renders the inner declaration with its
anonymous name restored, and lowering hands it back to
`build_sigilless_bind_stmt`, which splits it again.

A `my $ = EXPR` in expression position is a `VarDeclaration::Anonymous` with
rakudo's default scope (no `scope` field), no longer a `VarDeclaration::Simple`
named `__ANON_STATE__`; lowering gives it a non-`state` declaration.
`VarDeclaration::Simple.name` answers the full spelling (`$x`, `$*x`).

### 6.50 S10: `has $!g OP= EXPR` (2026-10-13)

A `has` declaration whose operator is not one of its initializers (`//=`,
`+=`, ...) declares the attribute without one and applies the operator to it
as an ordinary compound assignment (#8441 gap 2). Rakudo 2026.09 renders it
like `my $x OP= EXPR`: `ApplyInfix(left => VarDeclaration::Simple(scope =>
"has"), infix => MetaInfix::Assign(Infix(OP)), right => EXPR)`. In a
spelling-keeping parse the parser now builds the same record for a private or
alias attribute (`SourceForm::DeclarationCompoundAssign`, the assignment's
`Expr::CompoundAssign`), and `decl_compound_assign::parts` accepts a
`HasDecl`. The expansion lives in `parser::attribute_compound_assign`, which
both the parser and the lowering call. A public `$.g` writes through its
accessor (`Expr::AssignExpr`) and keeps no record, so it stays the boundary.

### 6.51 S10: `will PHASE` traits on a variable (2026-10-13)

`my $x will begin { ... }` runs the block in its phase with `$_` bound to the
container. The parser expands each trait into a phaser after the declaration
(`parser::wrap_with_will_phasers`: a `Phaser` whose body is a `given` of the
container or, for a compile-time phase, of the initial value). Rakudo 2026.09
keeps them on the declaration: `VarDeclaration::Simple(traits =>
(Trait::Will(phase => "begin", block => Block),))`, the block marked
`may-have-signature`. `RakuAST::Trait::Will` is a new model class;
`ast::var_will` reads the expansion back and holds the one phase-name table
the parser also uses, and lowering strips the traits and hands the phasers to
`wrap_with_will_phasers`. The parser reads `is` traits both before and after
`will` ones without recording their order, so a declaration with both stays
the boundary rather than render a guessed order.

### 6.52 S10: `require MODULE <imports>` (2026-10-13)

Rakudo renders the import list as `Statement::Require(module-name => ...,
argument => QuotedString(words))`. The converter now keeps the one-list form
(any other operand shape stays the boundary) and lowering passes it back as the
call's second operand. The caller names of an `EVAL` string include the loaded
modules' packages, since a package is global rather than lexical to the frame
that runs the `EVAL`.

### 6.53 S10: `::(EXPR)` declarator names (2026-10-14)

The parser keeps a computed declarator name as the declaration's `name_expr`
(plus a placeholder name unless the expression is a literal or bare word).
Rakudo 2026.09 renders it for `Sub`, `Method` and `Class` alike as
`name => Name(Part::Empty, Part::Expression(EXPR))`. The converter swaps the
`name` field for that form (`symbolic_deref::replace_declarator_name`) and
lowering reads it back (`routine_lower::declarator_name`); the package head
no longer mistakes the leading empty edge of such a name for a `class ::Foo`
stash. A grammar with a computed name stays the boundary. A name that only a
constant fold resolves (`my constant cname = 'X'; class ::(cname) { }; X.m`)
is still an unknown bareword to the converter.

### 6.54 S10: zen slices with `:exists` (2026-10-14)

The parser models `@a[]:adverb` as an `Index` with a Whatever index, except
when `:exists` / `:!exists` follows the brackets directly: then it keeps
`ZenSlice` under the `Exists` expansion. `ast::subscript_adverb::expand`
accepts `ZenSlice` when an `exists` adverb is present, the converter reads it
back as a subscript with no dimension, and lowering produces `ZenSlice` when
the first colonpair is `exists` (an `Index` otherwise, as before). An explicit
`:exists(True)` and the multi-dimensional `:exists:delete` error builtin stay
the boundary.

### 6.55 S10: attribute `will PHASE BLOCK` traits (2026-10-14)

An attribute's `will lazy { ... }` is kept by the parser as an unknown trait
`("will", phase, Some(AnonSub))`. The converter renders it as
`Trait::Will(phase, block)` in the attribute's `traits`, and lowering turns it
back into the same unknown-trait tuple, so `trait_mod:<will>` receives the block
as its positional Callable. A `will` trait without a block stays the boundary.

### 6.56 S10: compound assignment with a loose word-logical tail (2026-10-15)

For `$x += 1 andthen 42`, the statement parser keeps a scopeless
`SyntheticBlock([Expr(CompoundAssign), Expr(Binary(Var("x"), andthen, 42))])`.
Rakudo's tree is an `ApplyListInfix` whose first operand is the compound
assignment. The existing declaration-tail round trip now also recognizes a
scalar compound-assignment head, replaces the tail's leading variable read
with that head in `.AST`, and reconstructs the same parser shape in `lower`.
This pins the compound assignment as the value that seeds the short-circuit
tail and preserves its single evaluation. Other `SyntheticBlock` first
refusals remain for separate measured slices.

### 6.57 S10: grouped declaration with a loose word-logical tail (2026-10-15)

For `my ($a, $b) andthen "y"`, the parser stores the `SignatureDecl` expansion
inside the first statement of the loose-tail `SyntheticBlock`; the tail reads
the declared variables as an `ArrayLiteral`. Rakudo renders the declaration
itself as the first operand of `ApplyListInfix`. The converter now replaces
that comma-list seed with `VarDeclaration::Signature`, and lowering restores
the same declaration expansion and array seed. This record covers bare groups
of plain variables; initialized, nested, named and typed groups remain on their
existing paths or boundaries.

### 6.58 S10: sigilless call-assignment initializer under `quietly` (2026-10-15)

For `quietly my \z .= Numeric`, the parser stores a `DoStmt` around the
sigilless declaration expansion (`VarDecl(Mu.Numeric)` followed by
`MarkSigillessReadonly`). Rakudo renders `StatementPrefix::Quietly` around a
`Statement::Expression` containing `VarDeclaration::Term` with
`Initializer::CallAssign`. The converter recognizes this exact untyped
readonly expansion, and lowering restores its parser shape. Other sigilless
call-assignment expansions remain outside this boundary.

### 6.59 S10: `done VALUE` in a supply block (2026-10-15)

The parser desugars `done VALUE` to a `SyntheticBlock` containing `emit VALUE`
and `ReactDone`; the `supply` source record keeps that block in its written
body. Rakudo renders those as separate statements in the supply's block. The
statement-list converter now recognizes this exact two-statement expansion
and renders both nodes; lowering then rebuilds the supply body and its emitter
rewrite. Other synthetic blocks remain unchanged.

### 6.60 S10: lexical `&emit` in supply blocks (2026-10-15)

RakuAST lowering reconstructs a supply expansion after the parser has left the
lexical scopes that classified `emit` and `done`. Lowering now maintains a
temporary parser lexical scope for each block and registers its lowered code
bindings as statements are read. The supply expansion is rebuilt while its
body's scope is active, so a lexical `&emit` keeps shadowing the supply
rewrite. The same mechanism covers enclosing block declarations and lexical
sub declarations without changing how ordinary parsing builds a supply.

### 6.61 S10: `take` with a loose word-logical tail (2026-10-15)

For `take VALUE and TAIL`, the parser captures `VALUE` once, emits `take`, and
evaluates the logical tail against the captured value. Rakudo's tree keeps the
written form as `ApplyInfix(Call::Name("take"), TAIL)`. The parser expansion
now records the value, operator, and right operand in a `SourceForm`; conversion
renders the measured infix shape, and lowering calls the same expansion builder
as parsing. The regression pins single evaluation and short-circuit behavior.

### 6.62 S10: signature-literal binding (2026-10-15)

For `:(...) := RHS`, the parser evaluates the RHS once, binds it through the
signature binder, then binds each named variable to its corresponding value.
Rakudo renders the source as `ApplyInfix(FakeSignature, Infix(":="), RHS)`.
The parser expansion now records the signature and RHS in a `SourceForm`; the
converter renders that infix shape and lowering calls the same expansion
builder. The reverse operator map recognizes `:=` as `TokenKind::Bind`, and the
regressions cover type errors, multi-parameter binding, and one-time RHS
evaluation.

### 6.63 S10: goto statement (2026-10-16)

`Stmt::Goto` converts to the RakuAST shape measured in Rakudo: a
`RakuAST::Statement::Expression` containing `Call::Name::WithoutParentheses`, with a bare label as
`Term::Name` and a computed target as an ordinary expression. Lowering maps that call back to
`Stmt::Goto`. The focused structural regression is `t/rakuast/rakuast-goto-statement.t`; the
ordinary execution regression remains `t/control/goto-statement.t`.

### 6.64 S4: anonymous destructuring parameter modifiers (2026-10-17)

An anonymous positional destructuring parameter may carry an outer default,
`where` constraint, or `is` trait alongside its nested `Signature`. The
converter now preserves those fields in Rakudo's measured order, and lowering
reuses the existing parameter binder for where constraints and traits. The
focused regression checks the field presence, default omission of `optional`,
constraint enforcement, and EVAL of the constraint and trait forms.

### 6.65 S4: shaped array parameters (2026-10-17)

Rakudo stores the generated shape predicate block in `Parameter.array-shape`,
but omits that implementation detail from the constructor-form gist. The
converter now builds the measured `.list.ACCEPTS($_.shape).Bool` block for each
dimension expression, and the renderer keeps it out of `gist` while accessors
retain it. Lowering recognizes that predicate shape and restores the existing
`ParamDef.shape_constraints`, so the VM binder continues to enforce dimensions.
The regression covers one and multiple dimensions, lexical dimensions, and
mismatched shapes.

### 6.66 S4: anonymous subset types as keyed-hash keys (2026-10-17)

Rakudo represents `subset :: of Base where P` in a keyed-hash `shape` as a
structured `Type::Subset`, while the parser's keyed-hash binder consumes the
original type spelling. Parsing now retains both forms: conversion preserves
the measured subset node and its predicate, and lowering uses the retained
source to rebuild the keyed-hash constraint. The focused regression checks
the RakuAST tree, accepted and rejected keys, and the value-type archetypes.

### 6.67 S4: hidden and type-valued routine traits (2026-10-17)

Routine `Trait::Is` uses its `name` field for named traits such as
`hidden-from-backtrace`, and its `type` field for type-valued traits such as
`is Foo::Bar(:tag<x>)`. Conversion now preserves that distinction for subs,
anonymous subs, and methods; lowering maps a type-valued trait back to the
existing routine trait binder while the hidden marker remains internal. The
regressions cover the measured fields, named and anonymous dispatch, method
dispatch, and hidden backtrace frames.

### 6.68 S4: native package declarations (2026-10-17)

Rakudo models `native` declarations as `RakuAST::Native`, a `Package` node
with a name, optional scope, representation, source-ordered traits, and body.
Conversion now preserves that node and keeps mutsu's native-declaration marker
internal. Lowering restores the marker and routes `ctype`, `unsigned`, and
`nativesize` through the existing NativeHOW trait handling. The focused
regression covers tree shape, construction, and native type semantics.

### 6.69 S10: hyper call assignment to a nested subscript (2026-10-17)

For `@b[0]>>.=uc`, Rakudo exposes `ApplyDottyInfix` over the original
subscript, with `MetaPostfix::Hyper(Call::Method(dispatch => ".="))` on the
right. The parser expands this into a temporary-index writeback block. The
converter now recognizes that expansion structurally and restores the measured
tree; lowering routes it through the existing `.=` expansion with a hyper
method call. The regression checks the node shape and mutation after EVAL.

### 6.70 S10: zip metaassignment (2026-10-17)

Rakudo models `@a Z= rhs` as `ApplyListInfix` with
`MetaInfix::Zip(Assignment(:item))`. The parser's internal
`__mutsu_zip_assign` marker now converts back to that source-level node for
ordinary assignments and declaration assignments. Lowering restores the
existing element-wise assignment path, including the scopeless declaration
expansion needed by `my int @native[2;3] Z= 0..5`. The focused regression pins
tail preservation, fresh-array behavior, and row-major shaped native filling.

### 6.71 S10: placeholder scope rejection in supply and start blocks (2026-10-17)

Placeholder-bearing `supply` blocks can be desugared to a `DoBlock` before
RakuAST conversion. Parsing now retains a source-form marker on that block, so
conversion restores the measured `StatementPrefix::Supply` node and lowering
returns to the existing placeholder-scope check. `start` blocks with the same
scope error also retain their prefix through the desugared-block path. The
focused regression checks valid supply execution and the normal
`X::Placeholder::Block` diagnostic for both prefixes.

### 6.72 S10: hidden role traits (2026-10-18)

Rakudo renders `role R hides Parent` as `Trait::Hides(Type::Simple)` and
`role R is Parent is hidden` as `Trait::Is(type => ...)` followed by
`Trait::Is(name => "hidden")`. The parser already retains the hide relationship
and hidden-role marker as leading `DoesDecl` entries. Conversion now folds a
parent plus its hide marker into one `Trait::Hides`, and lowering rebuilds the
same entries from the measured trait nodes. The focused regression checks both
tree shapes, EVAL, and the resulting unhidden MRO.

### 6.73 S10: multiple inline declarations in a comma list (2026-10-18)

Rakudo renders adjacent inline variable declarations as separate operands of a
comma `ApplyListInfix`. The parser groups adjacent declarations in one
synthetic block, so conversion now emits one `VarDeclaration::Simple` per
operand. Lowering restores the synthetic block when the list is the target of a
subscript store, where Rakudo's postfix node omits the source parentheses, and
keeps individual declaration terms for metaassignment targets. The focused
regression checks the measured tree, the subscript store, and hyperassignment
to the declaration list.

### 6.74 S10: computed package name adverbs (2026-10-18)

Package declarator adverbs now convert their retained `Expr` values as the
`ColonPair::Value` value and lower those values back through the normal
expression converter. The parser also keeps a source-form marker for
parenthesized adverbs, since a literal expression there otherwise normalizes
to the same `Expr` as an angle adverb. Angle forms remain words quotes, while
parenthesized forms retain a `Circumfix::Parentheses` around their expression.
The regression checks class and role declarations, angle and computed forms,
source order, and evaluated metadata.

### 6.75 S10: qualified package names with adverbs (2026-10-18)

Package name adverbs now append `colonpairs` alongside an existing qualified
`Name.parts` field. Lowering removes the adverbs while preserving the parts,
then restores the parser's package header. The regression checks qualified
class and role names, their adverbs, and EVAL of the resulting declarations.

### 6.76 S10: explicit empty attribute deprecation messages (2026-10-18)

An attribute's `is DEPRECATED('')` now retains its explicit parenthesized
argument as `Trait::Is`, distinct from bare `is DEPRECATED`, which has no
argument and uses the default message. The regression checks both RakuAST
shape and the introspected message after EVAL.

### 6.77 S10: computed arguments to `also does` (2026-10-18)

Rakudo renders `also does R[1 + 2]` as a `Statement::Also` with a
`Trait::Does` whose `Type::Parameterized.args` contains the ordinary
`ApplyInfix` expression. The parser already retains both the argument `Expr`
and the complete role application spelling; conversion now carries those
arguments into the same parameterized type node used by declaration headers.
Lowering already rebuilds the `DoesDecl` with its argument expressions, so role
composition continues through the existing compiled declaration path. The
focused regression checks the node shape, EVAL, and the composed role's value.

### 6.78 S10: export traits on regex declarations (2026-10-18)

Rakudo represents `is export` on token, rule, and regex declarations as a
`Trait::Is(name => "export")`, with tagged exports carrying the tag list as
the trait argument. Conversion now preserves those traits from the parser's
declaration flags, and lowering maps them back to the existing export
registration. The declaration constructors also retain a `traits` list, so
hand-built nodes follow the same path. The regression checks the measured node
shape, tagged export metadata, EVAL, and hand-built trait lowering.

### 6.79 S10: named submethod declarations (2026-10-19)

Rakudo renders named `my`, `our`, and unscoped submethods as the same
`RakuAST::Submethod`, with no scope field. Conversion now omits scope for every
submethod, and lowering restores the parser's internal non-inherited marker
from the node class. EVAL keeps the declaration on the existing submethod
method path. The focused regression checks the node name, signature, omitted
scope, and callable result for all three source forms.

### 6.80 S10: variable-backed attribute and method handles (2026-10-19)

Rakudo represents `handles @names` as `Trait::Handles(Var::Lexical)`. The
parser already retained the variable term, but its handle-spec reconstruction
recognized only scalar variables and rejected array and hash variables. Those
terms now retain the same dynamic expression for attribute and method
delegation. The regression checks both measured trait nodes and EVAL forwarding.

### 6.81 S10: type-valued `is` traits on declarations (2026-10-19)

Aggregate variable and attribute declarations now preserve parameterized type
applications in `Trait::Is(type => Type::Parameterized)`. Variable declaration
traits also recognize qualified names that resolve to a declared type, while
unresolved names and custom `is NAME(args)` traits remain explicit coverage
boundaries. Lowering accepts the same parameterized type node and restores the
parser's existing type spelling, so execution stays on the normal declaration
path. The focused tests check qualified and parameterized type nodes, EVAL, and
the existing tied-container and generic-attribute behavior. The frontend
ratchet now lists 7,939 of 7,984 files.

### 6.82 S10: `:_` attribute declarations (2026-10-19)

Rakudo measures `has Int:_ $.x` as a `VarDeclaration::Simple` with the simple
`Int` type node; `:_` adds no definedness refinement to a declaration. The
attribute converter now omits that no-op smiley while preserving `:D` and `:U`
as `Type::Definedness`. Lowering therefore uses the existing typed-attribute
path, including its type-object default. The regression checks the measured
node, EVAL of an undefined default, assignment of a defined value, and the
existing NativeCall constructor test for the full smiley family. The frontend
ratchet now lists 7,944 of 7,988 files.

### 6.83 S10: anonymous declaration provenance (2026-10-20)

`Stmt::VarDecl.anonymous` records whether the parser consumed a bare sigil,
independently of the generated binding's spelling. The parser's implicit
anonymous-state declarations and the RakuAST anonymous-declaration lowerer set
the same flag; named RakuAST declarations clear it. Declaration rewrites carry
the flag, including sigilless initializer splitting, statement modifiers,
feeds, zip assignment, and call-assignment source forms. Conversion uses this
provenance both to omit implicit declarations and to select
`VarDeclaration::Anonymous`, so a legal source name such as
`$__ANON_STATE_9000__` remains a `VarDeclaration::Simple` with its initializer.
The serialized AST cache format is bumped for the new field.

Execution also used the old identifier-shaped names as state-store and method
invocant sentinels. Anonymous scalar storage now uses `__ANON_STATE#` (the
shared anonymous/invocant slot), `__ANON_STATE#<id>__`, and
`__ANON_STATE#PC_<id>__`. The `#` cannot occur in a source identifier. All
parser, compiler, VM, and signature consumers use the same spellings, so named
variables no longer enter anonymous-state read/write or per-call reset paths.
The `VarDecl` flag remains necessary to distinguish an explicit anonymous
declaration from its implicit state predeclaration, and travels through
declaration rebuilding rather than being reconstructed from user text.

Typed anonymous declarations now use the same initializer conversion and
declaration lowering as named declarations, retaining `type`, assignment,
binding, and call-assignment initializers without a second binder. Their
constructor preserves the same fields and defaults to `my` scope. Compiler
container decisions consume the declaration's anonymous flag, so an explicit
name cannot opt into anonymous scalar wrapping or lose its type constraint.

The lifecycle regression also exposed state-key collisions across separate
EVAL compilations: package, variable name and bytecode offset did not identify
a declaration uniquely. Both statement and expression declarations now mint
their state site from `compiler::compile_session` (ADR-11756); a guard and its
initializer share that site, and repeated calls of the compiled routine keep
it. The key includes the session namespace, logical body package and bytecode
offset, so alternate signatures sharing one logical body also share its state.
Separate string and AST EVAL units receive distinct session namespaces.

The focused dual-oracle regression covers explicit names resembling all three
anonymous-state spellings, repeated `my` and `state` calls, actual anonymous
declarations and implicit state, and a hand-built named declaration. This is
declaration metadata on the existing lowered representation, not a new
execution path or a converter reconstruction from a name.

The tested tree passes 8,036 / 8,079 frontend files; all 8,036 listed files
and 83,177 assertions pass the release ratchet. The 51 added list entries
include refreshed-main coverage. The remaining survey has 27 DIFF and
16 REFUSE files.

### 6.84 S10: declaration feeds and native variable taps (2026-10-20)

Both directions retain declaration sinks inside `Expr::Feed` until compiler
lowering, so conversion exposes `ApplyListInfix(Feed, operands)` rather than
the old `__mutsu_feed_array_assign` initializer. Feed lowering precedes the
generic loose-tail declaration split: a sink is part of the feed, not a
declaration initializer wrapped around it. `ApplyListInfix.new` preserves a
validated list of operand nodes, including hand-built declaration feeds.

The compiler evaluates a variable feed's source once, appends through the
existing native method table, discards the append result and returns the
original source. A raw compiler temporary and zen deitemization let a
scalar-held aggregate iterate into the sink without changing the original
expression result. Callable sinks retain ordinary argument evaluation order;
their existing arguments precede the appended source argument. The compiler
owns callable and legacy append-feed expansion; parsing retains the feed.

A typed mutable AST visitor hoists bare declaration sinks outside transparent
statement modifiers while leaving their appends in the modified body. It
stops at actual lexical scopes. Thus a false condition still installs the
typed declaration, and a loop accumulates successive inputs. `state` keeps
its own once-only declaration machinery and appends on each invocation.

The old feed-assignment runtime helper is removed. Array append and prepend
reject a single infinite range through their common native growth operation.
Subscript/method-expression sinks are refused at the RakuAST boundary, matching
Rakudo 2026.07's routine-call-or-variable restriction. Append feed syntax stays
refused because Rakudo has not implemented it. The executable contract is
`t/rakuast/rakuast-declaration-feeds.t` and the existing feed operator suite.

On the tested tree, 8,052 / 8,094 files pass the frontend survey, and all
8,052 ratchet files / 83,488 assertions pass under the release binary. The
16 added entries comprise the two feed tests and 14 refreshed-main tests;
the remaining cause survey has 27 DIFF and 15 REFUSE files.

### 6.85 S10: package scalar binding authority (2026-10-20)

TRIR outer seeding now consults the declaring package's canonical `our`
scalar cell after exact activation captures and before file/env fallbacks.
The cell remains live rather than entering a value snapshot cache. The
ordinary parser's incidental callsite marker had hidden this first-call
binding error; RakuAST lowering need not recreate that marker.

VM plain stores resolve the same cell before the generic bare-env write
shortcut. Container arguments carry that cell through `WrapVarRef`; the
parameter binder preserves an already-resolved lvalue instead of re-resolving
its name and scheduling an unrelated name-based copyback. Readonly decisions
and scalar constraints consult the target binding. `DeclareOurScalar` carries
its own optional constraint constant, including an explicit absence, rather
than attaching a caller's same-named type. Split runtime package bodies carry
their own `our` aliases separately from their private lexicals: compilation
hides caller slots for both, but binds only private lexicals from the static
store. The cache format advances to 38.

Class-body exit restores the bare aliases of top-level `our` declarations
while preserving their qualified storage and changes to shared outer cells.
The executable contract is `t/rakuast/rakuast-package-our-bindings.t`, with
string and AST EVAL coverage measured against Rakudo 2026.07. Existing routine
package-switch and in-file package-variable regressions exercise both
frontends as well.

Package declarations retain their written `is` traits and executable trait
arguments in the internal AST. Conversion, construction and lowering preserve
the same Module/Package fields, including written trait order beside export
traits and unit bodies. The compiler evaluates arguments and emits
`ApplyPackageTrait`, whose VM handler uses compiled `trait_mod:<is>` dispatch.
Missing parents and unsupported core HOW operations raise the shared typed
exceptions. Qualified declarations register their namespace ancestors;
unresolved qualified terms fail instead of manufacturing package objects.
Execution and conversion share the generated CORE type-name catalogue.
Generated export namespaces have separate unit and qualified-package indexes;
they do not become user package declarations or change custom EXPORT lookup.
An ancestor vivified during a pending class's load does not replace its HOW.
Class/role plans explicitly record compiler
qualification instead of guessing it from a repeated package prefix.
The matching method/body chunks use the same declaration-name provenance.
Relative compound type identities remain distinct from their written package
bindings; a registry identity does not manufacture another bareword binding.
A reverse index resolves qualified regex subrules through the public grammar
binding to that identity, for static patterns, callable token definitions and
candidate-less protos. Binding changes invalidate the existing regex caches.
Literal package stash subscripts remain index expressions rather than becoming
qualified-name lookups, so an absent member yields Nil. EVAL's caller namespace
classification includes the generated export indexes without declaring them
as user packages.
The static module scan reads retained literal stash assignments as export
names without executing BEGIN or an initializer. Ancestor publication belongs
to the package-kind operation and carries explicit public/private provenance.
The existing relative-class regression now measures constructor dispatch
against Rakudo instead of accepting a fabricated package value's name.
Post-declaration checks include modules and packages, and semicolon forms
raise the corresponding unit-scope syntax exceptions. These contracts are
measured in `t/rakuast/rakuast-package-namespaces-traits.t`.

Package/module declarations used as terms retain their declaration and lexical
scope through the existing compiler value path. `roast/S10-packages/basic.t`
now passes all 83 assertions through both frontends and joins the
whitelist; its completed blocker entry is removed.

Trait application uses the existing compiled declaration-registration timing;
the remaining compile-time argument/callback scheduling gap is measured in
#14146, including its pre-existing class reproduction.

### 6.86 S10: grammar argument expressions and binding windows (2026-10-21)

The regex source renderer retains `$<name>` and the distinction between named
fat-arrow arguments and positional Pair data, including nested calls. Pattern
specialization substitutes only already-bound enclosing parameters; it never
evaluates the argument expression against empty captures. The compiled match
engine evaluates the complete argument list as a Capture at the actual call,
using the ordinary compiler's named/positional/flattening rules. Binding
windows classify Pair data by its existing value marker, so an object inside
a positional Pair stays live across the callee's match and backtracking.

Grammar construction shares the Class/Native package-body contract, and
regex/token/rule construction retains signatures, scopes and multiness.
`t/rakuast/rakuast-grammar-argument-bindings.t` covers both directions and
hand-built grammar declarations; the existing capture-exception and binding
window files exercise the same behavior through the round-trip frontend.

### 6.87 S10: role composition provenance and caller bindings (2026-10-21)

`Stmt::RoleDecl::header_order` records the written interleaving of the
separately stored parent, export, rw and custom-trait families. Both block
and unit role parsers record it before normalization; conversion orders the
public traits from that record, and lowering records the order of constructed
trait lists. The serialized AST cache format is bumped for the new field.
The role header parser is split out of the declaration-body parser.

Anonymous role nodes retain their already minted declaration name as private
model metadata, following the existing source-metadata pattern. Lowering
reuses it; a newly constructed node still allocates a fresh name. No global
counter is reset, and the metadata is absent from public constructor rendering.
Absolute type names retain their leading empty edge without changing type
capture names at unrelated lowering sites. Source-only parameterized type
arguments support plain string and numeric literals; parsed argument lists
continue through the ordinary expression converter.

The lowerer reuses ADR-14144's `does` parameter analysis for subs, methods and
closures, including hand-built routines. Parenthesized expressions retain
`Grouped`, so a grouped role call stays an ordinary coercion rather than
becoming a mixin initializer. The existing compiler and VM own all execution.
The outer parentheses of `:key(EXPR)` remain colonpair delimiters, so they do
not add a `Grouped` operand that would hide the argument's source container.
`Role.new` and `RoleBody.new` share the package-construction helpers and retain
their body, traits and parameterization. The measured executable contract is
`t/rakuast/rakuast-role-composition-boundaries.t`.

### 6.88 S10: regex declaration provenance and shared code scanning (2026-10-21)

Parsed regex declarations retain their existing verbatim declaration text as
private model metadata. Conversion and lowering carry it independently of the
execution tree; rendering the RakuAST constructor omits it. Named method-table
entries and anonymous Regex values display that source through `.gist` and
`.raku`, including after module caching and scope capture. Regex closure
construction shares the source handle alongside the signature, tree and topic.

The Rakudo oracle distinguishes provenance from a deparser: an ArgList edit
changes EVAL's matching but the resulting Regex still displays its original
declaration. The implementation follows that contract without executing or
reparsing the reflection text. Anonymous source boundaries use checked UTF-8
slice access and report a parse error if the boundary is invalid.

RegexTree code blocks reuse `regex_code_nested::code_block_body_len`. The
shared scanner recognizes identifier apostrophes and Unicode quotes, and both
it and main-slang whitespace use one bracketed-comment primitive. Delimiter
nesting is iterative, with byte and char offsets computed together. Regex
declaration conversion, anonymous declaration parsing and value-provenance
helpers now have separate small modules. The executable contract is
`t/rakuast/rakuast-regex-source-provenance.t`.

### 6.89 S10: format quote processors and preview setting types (2026-10-21)

Format quotes cross the boundary as `QuotedString(processors => <format>,
segments => (...))`, retaining interpolation segments. The parser and lowerer
share the executable format-call builder. The existing format primitive
checks its Str parameter, so word processing after format installation raises
the same binding failure as Rakudo; it does not stringify a List into a format.
Plain word processors retain a List for one element, while `:val` and angle
word literals keep their existing scalar behavior.

Additional 6.e setting types are generated by comparing the same reference
Rakudo's 6.d and 6.e catalogs, using the existing catalog walker. Conversion
consults these additions only for a preview unit. This restores the
`Formatter::Syntax` type operand without fabricating an unresolved package or
making the additions part of the default catalog. Quote declaration adverbs
also no longer collide with loop labels sharing a keyword prefix.

The executable contract is `t/rakuast/rakuast-format-quote-setting-types.t`.
The formatter grammar's symbol captures and anchors are measured separately
against Rakudo; its existing regression retains all assertions.

### 6.90 S10: conditional lvalues, type receivers and subscript adverbs (2026-10-21)

A statement modifier on an assignment inside a compound-assignment target
lowers to an assignment expression, preserving its writable container. The
existing compound-assignment expansion then writes through the taken branch
and leaves an untaken modifier as a no-op. This matches the ordinary parser's
execution shape without adding a separate evaluator.

Bare type operands round-trip through `Type::Simple` and lower to term
lookups. A method call on `Any` therefore uses the term namespace rather than
a same-named scalar slot; an EVAL unit also uses that namespace for a term
whose declaration is outside its compile unit. Constructor calls retain the
ordinary bareword key because they do not write their type receiver, and raw
constructor arguments must keep their source containers. These choices
preserve an immutable type receiver while leaving a lexical type alias or
sigilless binding dynamically resolvable.

An invalid multi-dimensional `:!exists:delete` subscript still raises
`X::Adverb`, but its parser expansion now retains the original index and the
ordered colonpairs as spelling. Conversion emits the public postcircumfix
tree; lowering uses the existing subscript-adverb expansion to restore the
same exception. The order is retained in both directions, including
`:delete:!exists`. The executable contract is
`t/rakuast/rakuast-lvalue-contracts.t`, alongside the existing three focused
regressions for conditional assignment, bare `Any` growth and combined
subscript adverbs.

### 6.91 S10: declaration and source boundaries (2026-10-21)

The parser accepts legacy alternate signatures and `supersede` routines that
the current Rakudo oracle does not parse. Their executable data is retained as
private fields on the public `Sub` node. A DOC phaser exposes Rakudo's ordinary
`Nil` expression, with its original phaser retained privately so `--doc` can
still execute it after lowering. A role augmentation retains the role package
kind and its existing legality checks.

Composition with a bare Whatever operand keeps the written `o` expression in
`Spelling::CompositionWhatever` while its executable closure remains the
parser's normal expansion. RakuAST conversion recovers the public infix and
lowering reuses the parser's composition wrapper. The new serialized spelling
bumps the cache format to 43. A heredoc whose marker closes an enclosing block
retains that lexical boundary privately on its public `QuotedString`, so
lowering preserves the existing scope diagnostic without executing source in
the analysis path. Private source fields are omitted from constructor output.

`t/rakuast/rakuast-source-declaration-boundaries.t` checks the public shapes
and executable round trips; the seven affected language test files run in
both frontends. The captured-closure regex case remains a separate RegexTree
boundary (#14350) because its `%<chars>=` alias is not represented by the
static tree.

### 6.92 S10: hash capture aliases in static regex trees (2026-10-21)

`%<name>=ATOM` and `%name=ATOM` now parse into the shared `RegexNode::NamedCapture`
with a hash kind distinct from scalar and array capture aliases. RakuAST uses
`Regex::NamedCapture(hash => True)` for this mutsu extension: the current
Rakudo oracle rejects hash variables in regexes as reserved, while mutsu's
ordinary frontend implements the spec's hash capture semantics. Constructed
nodes validate mutually exclusive array/hash kinds; lowering sets the existing
matcher token's `hash_capture`, so each iteration files its key in the same
execution path. The serialized node shape bumps the cache format to 44.
Static hash alias names are capped at 1,024 characters; larger names decline
conversion with a catchable boundary error.

The same source file also contains an intentionally invalid unpaired `~`.
Structural regex validation now recognizes a leftover tilde marker across
nested patterns before the RakuAST conversion boundary. The check only walks
the already parsed token tree, with bounded recursion; it does not evaluate
regex code. `t/rakuast/rakuast-regex-hash-capture-alias.t` covers source and
constructed nodes, hash filing, and the diagnostic. The language regression
`t/regex/regex-last-declines-compiled.t` runs in both frontends.
The frontend-mode TAP test now checks the newly supported alias in the main
unit and an EVAL unit. A converter-level unit test supplies a deliberately
unmodelled Regex value to keep the no-fallback invariant stable as source
coverage grows.

### 6.93 S10: remaining source and lowering distinctions (2026-10-21)

Eight previously divergent files now execute through the RakuAST frontend.
Nested `<< >>` and `« »` word quotes retain their own source delimiter in the
serialized spelling and become child `QuotedString` segments. Lowering flattens
the nested words through the existing quote-word expression path. The cache
format advances to 45 for the changed spelling variant. A regex smartmatch
ends its comparison chain before a following `eq`; later comparisons chain
over its Match result. Qualified and routine-valued source barewords retain
their parser binding as private node metadata, so enum values and post-declared
listops keep the ordinary frontend's behavior.

The whole-frame `p6stateinit` lowering runs after the RakuAST round trip, when
the rebuilt statement tree is complete. A `with` statement modifier binds a
bare block's placeholders to its topic. Uninitialized native declarations use
the parser's shared native default when rebuilt. The overloaded `<=>` candidate
uses the same `InfixFunc` dispatch as the ordinary frontend. The focused
`t/rakuast/rakuast-final-diffs.t` test checks quote read and write directions;
`t/rakuast/rakuast-final-diffs-frontends.t` keeps the eight complete language
regressions green in frontend mode.

The inline-proto `use` argument and `use trace` are handled in §6.94.

### 6.94 S10: inline proto imports and statement traces (2026-10-21)

The parser's transparent `SyntheticBlock(ProtoDecl, Use)` for an inline proto
argument now converts to Rakudo's single `Statement::Use` node with the proto
`Sub` in its argument list. Lowering extracts that proto into the same
transparent declaration-and-import sequence, then passes its code object to
the module's `EXPORT`. This also accepts a constructed use node with a direct
proto argument.

`use trace` already records exact statement text, source line and statement
attempt number during parsing. Each traced statement now carries that data in
private RakuAST metadata and restores the existing `Stmt::Trace` hook before
compilation. The public node shape and `.raku` output stay Rakudo-compatible;
no tracing runs during `.AST` analysis. The two remaining language files pass
in frontend mode, bringing the tracked corpus to parity in all ten files from
#14359. The frontend ratchet and the inline-proto read/write assertions in
`t/rakuast/rakuast-final-diffs.t` pin the behavior.

### 6.95 Routine scope is resolved from the node, not its constructor text (2026-10-21)

The scope discrepancy reported in #14264 against Rakudo 2026.07 no longer
reproduces in the current oracle: a parsed named Method and a constructed
Method with the same name and body both answer `has`. Rakudo 2026.09 resolves
an omitted scope from the node kind and presence of a name: a nameless
Sub/Method/Submethod answers `anon`, a named Sub answers `my`, and a named
Method/Submethod answers `has`. Explicit scopes override that default and are
kept in the model. EVAL leaves the accessor value unchanged.

The uniform node's field schema now declares `scope` on routines, while the
accessor computes a missing value from kind and name. The parser's `is_my` and
`is_our` flags distinguish explicitly scoped submethods from the unscoped
form. Conversion retains their semantic scope field, and lowering restores
those flags. Rakudo omits a Submethod scope from constructor text even when
explicit; rendering applies that rule without discarding the node's field.
It also elides an explicit Sub or Method scope equal to its derived default.
No source-provenance marker or second resolver is needed for the current
routine contract. `t/rakuast/rakuast-routine-scope.t` checks parsed and built
nodes, both frontend modes, constructor output, and values before/after EVAL.

### 6.96 S10: explicit existence values and hash hyperslices (2026-10-22)

The parser's `Expr::Exists` retains an explicit `:exists(False)` argument,
which the generic subscript-adverb expansion inverse cannot distinguish from
`:!exists` by truth value alone. Conversion now emits the explicit argument
as `ColonPair::Value` on the existing array or hash postcircumfix. This also
preserves explicit zero and dynamic values; lowering still uses the existing
subscript-adverb execution path.

The 6.e hash `Expr::HyperSlice` is represented as `Postcircumfix::HashIndex`
with a `Term::HyperWhatever` index and its `k`, `v`, `kv`, `tree`, `deepk` or
`deepkv` colonpair. Lowering restores the shared hyperslice AST node before
the generic subscript-adverb expansion. The focused
`t/rakuast/rakuast-subscript-exists-hyperslice.t` records the Rakudo shapes
and EVAL behavior. All 111 assertions in the array/hash `exists-adverb.t`
and hash `hyperslice.t` roast files now pass in frontend mode.

### 6.97 S10: dynamic multidimensional subscript adverbs (2026-10-22)

The parser executes a 6.e multidimensional `:$delete` through a ternary: the
false branch reads the original subscript, while the true branch calls the
by-name delete primitive. The existing inverse for static subscript adverbs
cannot recover that ternary's source form. RakuAST conversion now validates
the shared target and dimensions, then emits the original array or hash
postcircumfix with its ordered static colonpairs and a
`ColonPair::Variable("delete")`. This covers bare `:$delete`, value adverbs
such as `:k:$delete`, and existence adverbs such as `:exists:kv:$delete`.
The parser keeps the distinct `:delete(EXPR)` spelling only during RakuAST
analysis, so its condition converts to `ColonPair::Value` even when the
execution ternary is otherwise identical to `:$delete`.
The execution expansion remains in the parser/compiler path; lowering uses
the existing subscript-adverb expansion.

`t/rakuast/rakuast-multidim-dynamic-adverbs.t` records Rakudo's node shapes.
The 812 assertions in `roast/S32-array/multislice-6e.t` and 549 in
`roast/S32-hash/multislice-6e.t` now pass in frontend mode.

### 6.98 S4: anonymous callable tails and typed scalar slurpies (2026-10-22)

The parameter parser already recognizes a bare `&` as an anonymous callable,
but returned before parsing the ordinary `is`, `where`, and default tail.
Thus `:(& is raw = &say)` fell into a permissive signature-literal fallback
without declared parameters, which the RakuAST converter correctly refused.
The bare callable now uses the shared parameter-tail parser, retaining those
fields in the structured signature value and its RakuAST parameter.

Rakudo accepts `Code *$block` as a typed scalar slurpy while rejecting a
typed positional array slurpy such as `Int *@a`. RakuAST conversion now keeps
the type on scalar slurpies and retains that rejection for arrays and hashes.
The focused `t/rakuast/rakuast-anonymous-callable-typed-slurpy.t` compares
both node shapes with Rakudo. `roast/S06-signature/introspection.t` and
`roast/S06-signature/slurpy-blocks.t` now pass in frontend mode. Two other
signature files from the earlier survey were already green on current main.

### 6.99 S10: empty enum, signed Complex and temporized undefine terms (2026-10-22)

An empty angle enum retains its raw whitespace in `EnumVariantForm::EmptyWords`
so `my enum E < >` reads as a word-quoted term with a literal space. An empty
parenthesized enum reads as `Circumfix::Parentheses(SemiList())`. Lowering the
word-quoted form restores the empty body rather than inventing a member. The
serialized enum form advances the precompilation cache format to 47.

A Complex value with a negative or non-finite imaginary component now reads
as a whole `ComplexLiteral`; only a finite, nonnegative imaginary value with
positive real zero uses the `Postfix("i")` reconstruction. This preserves both
signed zeros of an angle Complex literal.

The parser's `temp undefine($var) = VALUE` save/restore node now converts to
the source `ApplyInfix(ApplyPrefix("temp", Call::Name("undefine")), Assignment,
VALUE)`. Lowering that shape restores the same `undefine_first` save/restore
node. The focused `t/rakuast/rakuast-residual-source-terms.t` records Rakudo's
read shapes and signed-zero EVAL behavior. The enum basic, negative-zero and
scalar undefine roast files now pass in frontend mode.

### 6.100 S4: itemized private attributes (2026-10-22)

`$@!items` and `$%!lookup` are `Contextualizer::Item` nodes over private
attribute variables. Lowering now accepts attribute nodes alongside lexical,
dynamic, package and anonymous variables, restoring the parser's existing
`Expr::Itemize` path for arrays and hashes. The contextualizer test checks both
attribute kinds through EVAL, and `roast/S32-exceptions/misc2.t` passes in
RakuAST frontend mode.

### 6.101 S4: sigilless pointy closure parameters (2026-10-22)

A single sigilless parameter on a `PointyBlock` must retain its `ParamDef` when
lowered. Collapsing it to `Expr::Lambda` preserved the term name but discarded
the parameter metadata the closure binder needs to alias an indexed argument's
container. Such pointy blocks now lower through `Expr::AnonSubParams`, matching
the ordinary parser's representation. The focused RakuAST test checks the
`ParameterTarget::Term` shape and array/hash element writes through stored,
code-variable, ampersand and immediate calls. The existing
`t/routines/signature/pointy-sigilless-param-element-container.t` regression
passes under the RakuAST frontend.

### 6.102 S10: superseded built-in class and role HOWs (2026-10-22)

Rakudo's `RakuAST::Class` and `RakuAST::Role` constructor forms do not render
the unit's `EXPORTHOW::SUPERSEDE` context. The converter therefore carries the
parser's effective built-in HOW in hidden `source-supersede` metadata, which
the RakuAST renderer omits. Lowering restores the parser marker so subsequent
class and role registration uses the superseding HOW without leaking it to a
different compilation unit.

### 6.103 S10: regex aliases to a variable (2026-10-22)

`$foo=ATOM` in a regex parses into the source tree as
`RegexExtension::VariableAlias`; only the unspaced spelling is modelled, so the
rebuilt text is the text that was written. Rakudo 2026.09 parses the alias and
keeps only `Regex::Interpolation($foo)`: the atom is dropped from its tree and
its match. The converter renders that same public node and carries the aliased
atom in a hidden `source-alias` field, which the renderer omits; whitespace
written after the alias moves onto the hidden atom, as it does in rakudo's
grammar. Lowering restores the alias, so the matcher reads the same text as
on the ordinary frontend. A hand-built interpolation has no hidden field and
stays a plain interpolation. What the alias should *match* is #14628: mutsu's
matcher reads its `=` as a literal. The executable contract is
`t/rakuast/rakuast-regex-variable-alias.t`; `roast/S05-capture/alias.t` passes
in frontend mode.

### 6.104 S10: normalization-form quotes (2026-10-22)

`q:nfc'…'`, `q:nfd'…'`, `q:nfkc'…'` and `q:nfkd'…'` produce the `Uni` subtype
they name (S02; `roast/S15-string-types/NF-types.t`, `NFK-types.t`). Rakudo
2026.09 has no shape to measure: its grammar rejects the adverbs and its
`QuotedString` refuses an unknown processor. A quote adverb that post-processes
the quoted text is what `QuotedString.processors` models (`words`, `val`,
`exec`, `format`), so mutsu reads the quote as
`QuotedString(processors => ("nfc",), segments => (StrLiteral(TEXT),))`. The
parser folds the quote into its `Uni` value while parsing, so `TEXT` is the
normalized text; lowering normalizes it again through the parser's
`normalized_quote`, which leaves it unchanged. No `.AST` consumer can meet the
shape on rakudo, which cannot parse the source. The executable contract is
`t/rakuast/rakuast-nf-quote-literals.t`.

### 6.105 S2: statement origins are source ranges (2026-10-22, [#11413](https://github.com/tokuhirom/mutsu/issues/11413))

First Stage 2 slice, taken from the §4 decision on source positions. The parser
records, on the line marker it already emits in front of every statement
(`Stmt::SetLine(line, SrcSpan)`), the byte range the statement occupies in the
unit's source: from its first character to its last, the leading whitespace and
the terminating `;` left out (`parser::primary::statement_span`, which follows
heredoc buffers back to the original text like `$?LINE`). `SrcSpan` does not
take part in a statement's `Hash`, so redeclaration identity is unchanged.

`convert` turns that into Rakudo's shape. A statement node's hidden `origin` is
a `RakuAST::Origin` whose `from` / `to` are *grapheme* offsets (Rakudo counts
`\r\n` and a letter with its combining marks as one position) and whose
`.source` is the one `RakuAST::Origin::Source` of the unit (`rakuast/unit_source.rs`),
holding the text (`.orig`) and a line-start index, so `.original-line($pos)` is a
binary search, O(log n). `Str.AST` installs its text as the unit source; a
frontend round trip (`MUTSU_RAKUAST`) has none and keeps only the line, which is
all lowering reads. A statement the parser could not place (`SrcSpan::NONE`: a
lambda's synthesized marker) collapses to the start of its line. The measured
ranges (`Statement::If 11..46`, `Statement::Expression 21..26`, graphemes,
CRLF, heredocs, pragmas) are those of rakudo 2026.09 and are pinned by
`t/rakuast/rakuast-origin-ranges.t`, which passes on both.

Known gaps, both rakudo-visible: a comment between a statement and its `;`
(`say 3 # c\n;`) is still inside the range, and nodes below the statement
(`Sub`, `Block`, `VarDeclaration`, `Call::*`, ...) have no origin; those arrive
with their construct families, as the parser starts building them. Code::Coverable
(#11413) waits on those families.
