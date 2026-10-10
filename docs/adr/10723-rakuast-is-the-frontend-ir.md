# ADR-10723: RakuAST is mutsu's frontend IR — the parser emits RakuAST, the internal AST becomes the lowered form

- **Status**: Proposed (2026-10-02). Supersedes [ADR-0011](0011-rakuast-model-layer-and-phasing.md)
  once accepted. Stage 0's mode and CI ratchet have landed (§6); Stages 1–4 have not started.
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

- **Macros, `quasi` and unquoting (ADR-0011 Phase 6)** — Rakudo drops them in 6.e. Not planned.
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
- **Source positions.** `RakuAstNode` carries no position; the internal AST carries line
  information as `Stmt::SetLine` markers. Error messages and `$?LINE` must not regress, so the
  tree needs positions (Rakudo keeps an `origin`) before Stage 2. Decide the representation
  before the first parser-emits-RakuAST slice.
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

EVAL's existing static source validation precedes conversion, so invalid
source retains its typed CHECK-time exception instead of a conversion refusal.
This ordering introduces no execution during validation: rejected units never
run ordinary statements, and valid units still convert and lower before
execution. The same validation runs after lowering to check names resolved
from spelled terms and dynamic exports. `t/rakuast/rakuast-eval-source-diagnostics.t`
pins both boundaries.
Mainline diagnostic conversion remains a separate residual path.
