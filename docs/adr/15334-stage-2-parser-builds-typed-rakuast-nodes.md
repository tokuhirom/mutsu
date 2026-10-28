# ADR-15334: Stage 2 — the parser builds typed RakuAST nodes for every family, and the executing round trip and `convert` reverse paths are deleted

- **Status**: Proposed
- **Date**: 2026-10-28
- **Deciders**: tokuhirom, Claude
- **Issue**: [#15334](https://github.com/tokuhirom/mutsu/issues/15334). Roadmap:
  [#7564](https://github.com/tokuhirom/mutsu/issues/7564).
- **Related**: [ADR-10723](10723-rakuast-is-the-frontend-ir.md) (the decision this
  carries out: §2.2 Stage 2 and the rest of Stage 3, §4 "typed parser nodes, with the uniform node as
  their view", §2.4 the frontend-cost budget), [ADR-0088](0088-rakuast-regex-boundary-tree.md) (the
  regex source tree), [ADR-0137](0137-typed-ast-visitor-for-analyses.md) (the visitor every pass over
  the lowered tree uses), [ADR-0026](0026-slang-activation-architecture.md) (parse-time slangs)

This ADR supersedes no decision. It refines ADR-10723: it fixes the end state of Stage 2, the order
of its slices, and the gate of each slice, which §2.2 and §4 left open. ADR-10723 stays `Accepted`.

## 1. Context

### 1.1 Where the pipeline is now

ADR-10723's stages did not finish in the order it listed. Stage 3 (the default flip) came before
Stage 2 (the parser emits RakuAST), because the maintainer decided on 2026-10-27 that mutsu has no
users to protect (§6.122, §6.126, §6.127). The `MUTSU_LEGACY` opt-out and the unspelled parse for
execution are gone. Every compilation unit — the main program, every `use`d module, every `EVAL`
string — now runs as

```
 Source ─► Parser (spelled parse) ─► Stmt/Expr ─► convert ─► RakuAstNode ─► lower ─► Stmt/Expr ─► compile
                 │                       │                        ▲
                 └─ 31 ParsedStmt +      └─ passthrough: a pure   └─ EVAL($ast), hand-built trees
                    6 ParsedExpr            subtree skips both
                    (typed nodes)           (ParsedLeaf, §6.109)
```

Three kinds of tree reach `lower`, and the cost and the duplication differ by kind:

| Tier | What it is | Executing path today | `.AST` today |
| --- | --- | --- | --- |
| T1 pure | `Expr`/`Stmt` that is already what the compiler gets (calls, operators, literals, plain variables, closures, kept conditionals/loops, 72 variants named by `passthrough/pure.rs`) | carried through as a `ParsedLeaf`; `convert` and `lower` do not run | `convert_expr_node` / `convert_stmt_node` render it |
| T2 typed | `ParsedStmt` / `ParsedExpr` (`src/ast/parsed.rs`): the parser builds the RakuAST shape and the one lowering desugars it | one lowering (`lower_parsed_stmt`) | `parsed_view.rs` renders it |
| T3 reconstructed | every other family: routine, package and attribute declarations, signatures (`ParamDef`), `use`/`need`/`import`, `enum`/`subset`/`proto`/`augment`/`trusts`, regex declarations, phasers, `react`/`whenever`, and the parser's `SyntheticBlock` desugarings with their `__mutsu_*` call spellings | the parser desugars; `convert` reverse-engineers a `RakuAstNode`; `lower` builds the tree again | `convert` |

T3 is the problem. Its executing path does the work three times (desugar, undo, redo), the undoing
is what makes the converter a heuristic (it recognizes a desugaring "by the shape of its
statements"), and every RakuAST gap since 2026-09 has been a marker the parser smuggles through the
tree for `convert` to find.

### 1.2 Inventory (measured on `main` b675e0db, 2026-10-28)

| What | Measure |
| --- | ---: |
| `src/rakuast/` | 45,589 lines in 143 files |
| `convert.rs` / `lower.rs` | 6,926 / 5,842 lines (`convert_expr_node` 1,632 lines, `convert_stmt_node` 1,166, `regex_node` 341, `parameter` 272; `lower_expr` 1,278, `lower_regex_node` 375, `lower_parameter` 343) |
| `rakuast` files that name both directions | 70 files, 31,611 lines (the satellites: `use_stmt.rs`, `bareword.rs`, `signature_decl.rs`, `routine_*.rs`, `regex_*.rs`, ...) |
| `parsed.rs` + `parsed_view.rs` (T2 nodes and their view) | 807 + 715 lines |
| `frontend.rs` + `passthrough.rs` + `passthrough/` (the executing round trip) | 148 + 383 + the `pure.rs`/`tests.rs` visitor |
| `Stmt` / `Expr` variants | 69 / 86 |
| `SyntheticBlock` references | 263 in 90 files; 59 producer sites in `src/parser` |
| `__…` internal-name literals in `src/parser` | 178 occurrences, 74 distinct |
| `__mutsu_*` literals in `src/` | 1,479 (147 in `src/compiler`) |
| `__mutsu_assign_{callable,method,named_sub}_lvalue` | 89 sites |
| `Stmt::Ast` / `Expr::Ast` references | 72 / 47 (21 producers in `src/parser`) |
| `crate::parser` references from `src/rakuast` | 178 (the desugaring `lower` shares with the parser) |
| `ci/rakuast-frontend-passing.txt` | 8,722 files at the `convert` level |

Frontend cost with the round trip as the default (ADR-10723 §6.109, callgrind Ir of `mutsu -c`,
profiling build, warm; ratio to the pre-flip ordinary frontend): 1.06 (`Zef/CLI.rakumod`), 1.04
(`S32-exceptions/misc.t`), 1.06 (`S12-class/attributes.t`), 0.99–1.08 on the other cases. The
budget (§2.4) holds, but only because T1 skips the round trip; the numbers say nothing about a
program that is mostly T3.

### 1.3 What the cheap deletions were, and why they stop

The deletions since §6.110 (traits, signature markers, the `VarMarks` fields, `Stmt::MarkBind` /
`MarkSigilless`, the legacy opt-out) all had one shape: a marker the converter read became a field
of the node it described, and the converter's scan of the marker was deleted. What remains does not
fit that shape. `SyntheticBlock` is the container for "one source construct, several statements",
and the converter recognizes the construct by the statements' shape; the `__mutsu_*_lvalue` call
spellings are recognized by name by both `convert` and the compiler. Neither is a marker on a node;
each is a desugaring whose source form the parser threw away. Removing one means the parser keeps the
source form (a typed node) and `lower` desugars it — that is Stage 2 itself, one family at a time.

## 2. Decision

### 2.1 End state

```
 executing parse:  Source ─► Parser ─► typed nodes, lowered where built ─► Stmt/Expr ─► compile
 .AST parse:       Source ─► Parser(keep spelling) ─► typed nodes + T1 Stmt/Expr ─► view ─► RakuAstNode
 EVAL($ast):       RakuAstNode ─► typed node ─► the same lowering ─► Stmt/Expr ─► compile
```

1. **Every desugaring has one site, in `rakuast`.** For each T3 family the parser builds a typed
   node shaped like the RakuAST node (ADR-10723 §4) and a single function lowers it. The parser stops
   desugaring that family: its `SyntheticBlock` producers and `__mutsu_*` call spellings go. The
   uniform node's lowering reads the node into the same typed struct and calls the same function, so
   a family never has a second lowering.
2. **The executing path is one pass.** An executing parse (main program, modules, `EVAL` strings, the
   lazily parsed fragments) lowers each typed node at the point the parser builds it. It does not
   keep spellings, does not build a `RakuAstNode`, and does not walk the unit again. `frontend::
   round_trip*`, `passthrough` (the `ParsedLeaf` scope and the purity visitor), `expr_origins::strip`
   and the second `qualify_type_names` pass are deleted at the cut-over (slice 9).
3. **`.AST` is a view, not a conversion.** A `keep_spelling` parse keeps the typed nodes in
   `Stmt::Ast` / `Expr::Ast` (the transitional carriers of ADR-10723 §4). The renderer reads what the
   parser built in RakuAST's shape; it never undoes a desugaring.
4. **T1 stays as it is.** An `Expr`/`Stmt` that is already lossless is its own typed node (ADR-10723
   §4: "the typed node is that `Expr` plus its positions"). Its renderer arms remain, renamed from
   "convert" to "view", and the term-origin bridge (`term_origins` → `Expr::Spelled`) stays for them.
   This ADR does **not** wrap the 86 `Expr` variants in new node types.
5. **`src/rakuast/convert.rs` is retired family by family.** What is deleted: every arm that
   reconstructs a source form from a desugared tree (the T3 arms, the `SyntheticBlock` recognizers,
   the by-name recognizers of `__mutsu_*` calls, the bareword/name classification that guesses what
   the parser resolved). What is kept, under the view's name: the T1 arms and the T2 view. The module
   boundary then reads "RakuAST in, `Stmt`/`Expr` out" (`lower`) and "typed node in, `RakuAstNode`
   out" (`view`); no function takes a desugared tree and returns a node.

What this ADR does not change: the compiler, the VM, TRIR, the JIT, the typed visitor over the
lowered tree, the precompilation cache format (it stores the lowered `Vec<Stmt>`), and the
`Value::RakuAst` / `RakuAstClass` representation. It adds no execution engine.

### 2.2 Migration: slices

One family per PR, as ADR-10723 §4 decided. Each slice moves a family's desugaring from the parser
into `lower`, makes the parser build the typed node, adds the node's view, and deletes the family's
`convert` arms and parser markers in the same PR. **A slice that is not net-negative in non-test
`src` lines of `convert.rs` + `lower.rs` + the satellites it touches + `src/parser`, or that leaves a
parser marker for its family, is not complete.** The order puts the families with the most
reconstruction first; the dependency notes are why the order is not by size alone.

| # | Slice | Families | Deletes (named, not promised sizes) |
| --- | --- | --- | --- |
| 0 | Gates | `scripts/frontend-cost.sh` (callgrind Ir of the four budget cases, §3.2), a ceiling ratchet `make check-frontend-debt` (§3.3) | nothing; adds the measurements every later slice reports |
| 1 | Expression call spellings | the `__mutsu_assign_{callable,method,named_sub}_lvalue` family (89 sites), `__mutsu_hyper_prefix`, the atomic `__mutsu_*_var` ops, the subscript-adverb calls | the by-name recognizers in `convert`, the compiler's by-name dispatch of the same spellings |
| 2 | `use` / `need` / `import` / `no` / `require` | `Stmt::Use` family, `use_stmt.rs` (703 lines), `use_origins.rs` | the shape-based reconstruction of imports; `unit_source.rs` readers |
| 3 | Signatures | `ParamDef` → typed `Signature`/`Parameter` node (the S4/§6.113–§6.116 fields are the start), `parameter`/`lower_parameter` (272 + 343 lines), the `parameter_*`/`signature_*` satellites | the placeholder-name recovery, the pseudo-trait filters |
| 4 | Routines | `SubDecl`/`MethodDecl`/`ProtoDecl`/`TokenDecl`/`RuleDecl`/`ProtoToken`, `routine_*.rs`, `declared_routines.rs` | the routine reconstruction, `__mutsu_prelude` (~160 `SubDecl` sites become a field or a typed prelude), the `my method` → `SubDecl` shims |
| 5 | Packages and members | `ClassDecl`/`RoleDecl`/`Package`/`HasDecl`/`DoesDecl`/`TrustsDecl`/`AugmentClass`/`EnumDecl`/`SubsetDecl`, `attribute*.rs`, `role*.rs`, `package_*.rs` | the header/body reconstruction, `NestedTypeShells`, `PackageRuntimeBody` markers |
| 6 | Declarations that expand to several statements | `my ($a, @b) = …` destructuring, `has`/`state`/`our` splits, modifier splits, `let`/`temp` | the remaining `SyntheticBlock` producers of the declaration kind |
| 7 | Concurrency and phasers | `react`/`whenever`/`supply`/`start`, `Phaser`, `DocPhaser`, `Catch`/`Control`, `Label`, `Goto` | the supply/react marker statements (`ReactDone`, `SupplyBodyDone`) |
| 8 | Regex declarations and literals | already a non-lossy tree (ADR-0088): move `regex_node` (341) / `lower_regex_node` (375) and the `regex_*` satellites under the view/lowering naming, delete only the reconstructions | regex placeholder and alias reconstructions that a typed node makes unnecessary |
| 9 | Cut-over | executing parse becomes unspelled with inline lowering (§2.1.2); `Stmt::SyntheticBlock` is removed (every producer is gone by slices 1–8) | `frontend::round_trip*`, `passthrough` and `ParsedLeaf`, `expr_origins::strip`, the `unspell` path, the spelled-parse switch for execution; the `convert`-level coverage mode folds into the `.AST` mode |

Slices 1–8 are independent of each other except where a table row says otherwise (3 before 4:
routines own signatures; 6 after 4 and 5: its splits sit inside their bodies). While slices 1–8 are
open the executing path is today's round trip; each slice only shrinks what goes through `convert`
and `lower`. **Only slice 9 changes how an executing parse runs**, and it can land only when no
`SyntheticBlock` producer is left, because the round trip is the only thing that currently reads
the parser's desugared shapes for T3.

Slices not in the table are filed as sub-issues of #15334 when found, never as a new table row
(the S1–S10 rule of #7564).

### 2.3 Parse-time knowledge

ADR-10723 §4 left "where compile-time knowledge lives" to be decided per construct. This ADR fixes
the rule for Stage 2: **a typed node records what the source said; `lower` consults the parser's
existing tables for what a name means, and Stage 2 adds no resolver.** `convert`'s bareword and
imported-name classification (`bareword.rs`, `CallerNames`, `core_term_names`) is the part that
guesses what the parser resolved; each slice that deletes a reader of it also deletes the guess, and
the slice that cannot (names an `EVAL` string takes from its caller) records the open item in its PR.
The cut-over (slice 9) is gated on that classification having exactly one site.

### 2.4 Safety invariants

Inline lowering must keep every property the parse has today:

- **Analysis parses execute nothing.** `parse_program_recovering`, `src/analysis/`, the LSP and
  `--dump-*` build the same typed nodes and lower them with the same pure function; no slice adds code
  execution to a path that only parses (`docs/security.md`).
- **Parse-time ordering.** Types, operators and slang modes the parser registers while parsing are
  registered when the node is built, as now; lowering at build time must not move that registration
  after the node's children are parsed.
- **Precompilation.** The cache stores the lowered statements; a slice that changes what lowering
  emits bumps the cache version, as any compiler change does.
- **Rust file size.** A new satellite stays under 500 lines; a slice splits what it grows.

## 3. Verification

### 3.1 Per slice

1. `scripts/dev gate` with `--focus` on the family's `t/rakuast/*.t` and the roast files that exercise
   it; the verdict `pass` before publishing (AGENTS.md).
2. `scripts/rakuast-frontend.sh check`: the 8,722-file list may only grow, at the `convert` level
   (which keeps exercising the arms the passthrough bypasses) until slice 9.
3. A coverage run over `t/` and the whitelisted roast files in `MUTSU_RAKUAST=convert` and the
   default mode (the method of §6.117, §6.118): every `convert`/`lower` arm the slice deletes was
   unreachable or replaced; the PR body names the arms and the coverage evidence. A zero-hit block
   is not proof of dead code (§6.112 found live untested syntax): probe it before deleting.
4. `.AST` text parity does not regress: `scripts/ast-text-corpus.sh` against rakudo, per-class
   counts in the PR body.
5. `scripts/frontend-cost.sh` on the four budget cases, before and after, in the PR body.
6. The deletion accounting of §2.2 (lines and the three counts below), measured with the commands
   in the PR body.

### 3.2 The cost budget

ADR-10723 §2.4: total frontend time within 10% of the ordinary frontend. The ordinary (unspelled,
no round trip) frontend no longer exists in the tree, so the baseline is **the callgrind Ir recorded
once, in slice 0, by building the commit before §6.122's flip**, on the cases of §6.109 (`Zef/CLI`,
`S32-exceptions/misc.t`, `S12-class/attributes.t`, `emoji-test.t`) plus one declaration-heavy file
(a `t/` class/role/grammar test) so a T3-dominated program is measured. Slices 1–8 may not raise the
Ir of any case by more than 2% each; slice 9 must bring the executing path to **at or below the
baseline**, since it removes the pass that the budget exists to bound.

### 3.3 The ratchet

`make check-frontend-debt` fails when any of three counts rises above its ceiling: `SyntheticBlock`
producer sites in `src/parser`; distinct `"__…"` internal-name literals in `src/parser`; lines of
`src/rakuast/convert.rs`. A ceiling file is one line per metric under `scripts/frontend-debt.d/`;
a PR lowers a ceiling when it lowers the count, and a PR that does not touch the count never edits
the file (AGENTS.md: a drop must not require a re-cut). Slice 9 deletes the check with the last
thing it counts.

## 4. Consequences

- **The executing path gets cheaper than today**, not merely equal: no spelled parse, no second
  tree, no walk. The budget stops being a constraint on how many families may be T3.
- **The ticket stream ends.** A RakuAST "renders the wrong node" gap becomes a field of a node the
  parser already builds; there is no heuristic left to be wrong about a shape.
- **The deletion is real but smaller than the headline.** Of the 12.7k lines of `convert.rs` +
  `lower.rs`, the T1 arms and the T2 view survive as `view`, and `lower` survives whole (it is the
  write direction `EVAL($ast)` needs). The deleted part is the reconstruction: the T3 arms, the
  recognizers, the bareword guesses, the executing round trip, `passthrough`, and the parser's
  desugaring code that `lower` now owns. Each slice reports its own number; this ADR promises no
  total, because the desugaring moves from `parser` to `rakuast` before it shrinks.
- **Risk — a long migration with a mixed state.** The mixed state is the one ADR-10723 §2.2
  defined and has been shipping since §6.109: typed nodes and unmigrated subtrees both reach `lower`.
  Each slice is behaviour-neutral by its gate; the default frontend is never switched off.
- **Risk — slice 9 is the one that touches every program.** It lands only with the ratchet counts at
  zero, the cost at or below baseline, and the full gate (`--full`) green. It is a revert-safe change
  to `frontend.rs` and the parse entry points, because the typed nodes are the same ones the earlier
  slices ran through the round trip.
- **Risk — `.AST` regressions in the corpus.** Mitigated by the parity check per slice; a slice that
  lowers the corpus score is incomplete.

## 5. Alternatives considered

- **Stop here: keep the round trip and `convert` as the permanent frontend.** Rejected. The
  executing path keeps a second tree and a heuristic reconstruction for every declaration, and the
  budget works only while programs are mostly T1. It also leaves the 178 parser references from
  `rakuast` and the 74 distinct `__…` literals in the parser as permanent debt.
- **Reinstate the unspelled parse as the executing path and keep `convert` only for `.AST`** (undo
  §6.127). Rejected: it is two frontends again, one of which `EVAL($ast)` and hand-built trees never
  exercise, which is the state ADR-10723 §1.2 set out of. The maintainer's decision of 2026-10-27
  removed it. Slice 9 reaches the same cost without the second frontend: the parser's output is the
  typed node, and the one lowering runs on it.
- **Cut over first (slice 9 first), then migrate families.** Rejected: until no `SyntheticBlock`
  producer is left, the unspelled executing tree still holds shapes only the converter understands.
  The cut-over would have to keep a round trip for exactly the T3 programs, which is the mixed state
  at a worse place.
- **A node type for every `Expr`/`Stmt` variant.** Rejected (ADR-10723 §4 types only the families the parser
  desugars): the 86 `Expr` variants that are already lossless gain nothing, and wrapping terms
  as they are parsed broke 900–1,300 of 8,451 files in the measurement behind the §4 amendment.
- **Compile directly from the typed nodes and delete `Expr`/`Stmt`.** Rejected (ADR-10723 §5):
  rewrites the compiler, TRIR, the JIT inputs and every analysis for no user-visible gain.
- **One large PR that migrates all T3 families.** Rejected on process, not size: the gate and the
  coverage evidence identify a family's dead arms; a combined PR cannot attribute a `.AST` or cost
  regression to a family. AGENTS.md prefers a real refactor over a token slice, and each slice here
  is a real refactor of one family, not a documentation of one.

## 6. Open questions

- **Typed node granularity for declarations.** A routine, a signature and a class body each have
  several RakuAST classes. Slices 3–5 decide whether the typed node is one struct per RakuAST class
  or one per source construct, by the rule "the parser builds nothing the source did not say";
  recorded in the slice's PR, not here.
- **`EVAL` string names.** The names an `EVAL` string takes from the scope that runs it are passed
  to `convert` today (`CallerNames`). The typed node records the bare name; the open item is whether
  `lower` or the compiler resolves it (§2.3). Decided in slice 2 or 3, whichever first needs it.
- **Modules before slice 9.** Modules are round-tripped today (§6.127). If a module's precompilation
  shows a cost slice 9 does not recover, a per-module decision follows the measurement; this ADR does
  not pre-decide it.
