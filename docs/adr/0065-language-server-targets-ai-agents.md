# ADR-0065: mutsu's language server targets AI agents, and is scoped to the protocol surface an agent actually consumes

- **Status**: Proposed (2026-09-02). Design only — no implementation has started.
- **Context**: The user asked for a language server built on mutsu, *for mutsu* — a tool
  for people (and agents) writing Raku that is meant to run on mutsu. This is a deliberate
  **new capability** direction with zero roast payoff, in the same category as ADR-0011
  (RakuAST): large, costly to reverse, and therefore designed before any code is written.

## Problem

Three questions have to be answered together, because each one's answer changes what the
others cost:

1. **Is mutsu a viable analysis frontend at all**, given that it was built to *execute*
   Raku, not to serve an editor?
2. **Which part of the Language Server Protocol do we implement?** The protocol is large,
   and the expensive methods are expensive for structural reasons.
3. **Where does the code live**, given that mutsu's parser and AST are entirely private?

### What mutsu already provides

Measured 2026-09-02 on this checkout (release build, single machine, one-off; **these are
feasibility measurements, not bench-CI performance numbers** — see the repo rule that
performance claims in documents come from `bench-history.tsv`):

| Measurement | Value |
| --- | --- |
| Process startup (`-e 'say 1'`) | ~6 ms |
| Parse of one ecosystem module, average over 249 files | ~9 ms |
| Parse of the largest bundled module (1711 lines) | ~100 ms |
| Cold load of `Cro::HTTP::ResponseSerializer` + its dependency closure | ~820 ms (rakudo: ~14,700 ms) |
| Same, with the precompilation cache warm | ~460 ms (rakudo: ~495 ms) |

Parsing all 249 `.rakumod`/`.raku` files under `modules/` and `vendor/` in isolation
(`--dump-ast`) succeeds on 237. The 12 apparent failures are **not parser defects**: 7 of
them are `when X::Some::Type { ... }` where the type is imported from a dependency, and
rakudo emits the *identical* error (`Function '...' needs parens to avoid gobbling block`)
when that dependency is absent. Loading the same file through `use` succeeds. This is the
single most important fact about Raku as an LSP target:

> **Raku's grammar is not context-free with respect to the imported symbol table.** To
> parse a file correctly you must first know what its dependencies export — their type
> names, their custom operators, their slangs. rakudo has exactly this property; it is not
> a mutsu limitation.

Two existing mechanisms matter here:

- `parser::parse_program_partial` (`src/parser/mod.rs:708`) already skips a failed
  statement and resumes at the next one.
- `src/parser/stmt/simple/module_exports.rs:486` already uses it to **harvest a module's
  exported symbols statically, without executing the module**.

Together these are the seed of an error-tolerant, non-executing frontend — the exact
capability that a compiler frontend (rakudo's, or mutsu's own execution path) cannot offer.

### What mutsu does not have

- **No positions in the AST.** `src/ast.rs` (3271 lines, ~50 `Expr` and ~30 `Stmt`
  variants) has no `span`/`line`/`column`/`offset` field on any node. The only positional
  information is `Stmt::SetLine(i64)` (`src/ast.rs:1455`), a *marker statement* interleaved
  into statement lists at top level and in brace-delimited block bodies. Columns do not
  exist anywhere. Expression-level positions are lost entirely.
- **No lexer.** The parser is scannerless: hand-written recursive descent over `&str`.
  There is no token stream and no trivia retention. (`token_kind.rs` is an operator tag
  enum stored in AST nodes, not a lexer token.)
- **One diagnostic per parse.** `parse_program` returns `Err` on the first failure and
  discards any partial result; `parse_program_partial` recovers but reports neither
  positions nor errors. There is no sorrows/worries accumulator.
- **The parser's lexical scope is discarded.** `SCOPES` is a thread-local stack of name
  sets with no positions, torn down when parsing ends.
- **Everything is private.** `src/lib.rs` declares `ast`, `parser`, `compiler`, `opcode`,
  `runtime`, `vm` as private modules; the only public surface is `Interpreter`,
  `RuntimeError`, `Value`, and `dump_ast`/`dump_bytecode`, which return `String`.
- **mutsu has never run as a long-lived process that parses repeatedly.** The REPL is the
  closest existing consumer and is orders of magnitude smaller in volume. The parser tracks
  the current source through `ORIGINAL_SOURCE`, a thread-local `(raw pointer, len)` pair
  (`src/parser/primary/mod.rs:31`); `src/symbol.rs` interns strings for the process
  lifetime; `CLASS_DECL_ID_COUNTER` grows monotonically.

## Decision

### D1. Build a language server, and treat an AI agent as the primary consumer

Editor tooling is increasingly driven by coding agents rather than by a human moving a
caret. We design for that consumer *first*. This is not a prediction about humans
disappearing; it is a scoping decision that determines which protocol methods earn their
implementation cost.

### D2. Speak LSP itself, not a bespoke interface

An agent-shaped interface (a CLI subcommand, or an MCP server) would fit the consumer more
naturally than a protocol built around caret positions and keystroke latency. We reject it
anyway: **agents already speak LSP**, so the protocol buys integration with existing
harnesses at zero cost to them, whereas a bespoke tool must be adopted one client at a
time. Where a capability has no LSP spelling, we add it as a mutsu-specific extension
method rather than replacing the protocol.

### D3. Implement only the methods an agent consumes

| Method | In scope | Rationale |
| --- | --- | --- |
| `publishDiagnostics` | **Yes — flagship** | "Did my edit break something" is the dominant agent query |
| `documentSymbol`, `workspaceSymbol` | Yes | Exact answers where an agent would otherwise grep |
| `definition`, `references` | Yes | Removes grep's false positives |
| `hover` | Yes (later) | Type/signature, and mutsu coverage status |
| `completion` | **No** | An agent does not type character by character |
| `semanticTokens` | **No** | Pure rendering |
| `signatureHelp`, `inlayHint` | **No** | Typing aids and rendering |
| Incremental document sync | **No** | Full reparse at ~9 ms is sufficient |

The two exclusions that matter most are structural, not cosmetic:

- Dropping `semanticTokens` **removes the need to write a lexer**, which mutsu does not
  have and would have to build from nothing.
- Dropping `completion` **removes the need for caret-position scope resolution** — the
  hardest form of positional analysis, requiring the parser's lexical scope to survive
  parsing and be queryable at an arbitrary offset mid-expression.

Dropping incremental sync deletes the document-diffing subsystem outright, and the
keystroke latency budget with it.

### D4. "Does mutsu support this?" is a first-class diagnostic

Because the target runtime is mutsu, "mutsu does not implement this method/routine" is not
a false positive to be suppressed — it is the single most valuable fact the server can
report. An agent writing Raku for mutsu has no other way to learn mutsu's coverage short of
running the code, and will otherwise emit unsupported constructs with full confidence.

This requires mutsu's built-in names to become **enumerable**. They are currently string
literals in `match method { ... }` arms spread across `src/builtins/methods_0arg/*.rs` and
`methods_narg.rs`. The fix is to derive the dispatch arms and a name table from one source
(a macro, or a name list asserted against the dispatch by a unit test) — never a
hand-maintained second list, which would drift.

Where mutsu knows a replacement, the diagnostic should carry it (LSP `CodeAction`, or the
diagnostic's `data`). For a human consumer this would be intrusive; for an agent it is the
point.

### D5. Correctness of the message text outranks precision of the range

An agent tolerates a range that is off by a few characters — it re-reads the line. An agent
does **not** tolerate a diagnostic that is wrong: it believes it and writes worse code. A
human treats a bad language server as noise and ignores it; an agent obeys it.

Effort therefore goes to *what the message says* rather than to sub-token range accuracy.
mutsu's existing error rendering (surrounding context, hints, "Did you mean" suggestions
from `src/runtime/undeclared_routines.rs`) is already the right shape and pays off directly.

The corollary is a real hazard: **an AI consumer absorbs imprecision silently, so quality
can rot unobserved.** Positional correctness must therefore be pinned by automated tests
from the first slice, never by looking at an editor.

### D6. Line granularity first; expression spans only where a feature demands one

We reject a big-bang retrofit of spans onto all ~80 AST variants. `Stmt::SetLine` already
yields statement-level lines, which is sufficient for `documentSymbol` and for
`definition` when the consumer reads the file anyway. Spans are added **per feature, to the
variants that feature needs** — declaration nodes first (~10), reference nodes second (~5)
— and only once a feature that cannot work without them is being built.

`references` is the first method that genuinely needs per-occurrence positions, because a
line may hold several. It is scheduled accordingly (S5), not assumed away.

### D7. Three layers, three homes

| Layer | Home | Why |
| --- | --- | --- |
| Spans, error recovery, multi-diagnostics, enumerable built-in tables | **mutsu core, in tree** | `ast`/`parser`/`compiler` are `pub(crate)`; this work is physically inside the parser and cannot be done from outside |
| The server (JSON-RPC, document state, LSP method handlers) | **A workspace crate in this repository** | Must track mutsu's parser in lock-step — a separate repository would break silently on parser changes, whereas in-tree CI catches it. Also keeps `tower-lsp`/`tokio` out of mutsu's own dependency tree, in the style of the existing `native`/`wasm`/`jit` feature split |
| Editor extensions (VS Code, etc.) | **Separate repository** | Different language, different registry, different release cadence |

mutsu is currently a single crate with no `[workspace]`; adding the server means splitting
one out. That refactor is part of the cost.

The core-layer work is **not** an LSP-only tax: column-accurate positions, multiple
reported errors, and error recovery improve mutsu's own diagnostics and its `EVAL`
checking. The server is additionally the best available regression test for positional
correctness — a class of defect roast cannot detect.

### D8. Long-lived-process viability is a gate, not a later concern

Before the server skeleton, measure mutsu parsing the same document thousands of times in
one process: resident memory, `symbol.rs` interner growth, and the behaviour of
`ORIGINAL_SOURCE`'s thread-local raw pointer under repeated re-entry. Single-threaded
sequential parsing should follow the existing `EVAL` path and hold; concurrency across
documents almost certainly will not, and the server must stay single-threaded for parsing
until that is addressed. If this gate fails, the rest of the plan is invalid, so it runs
first.

## Rejected alternatives

- **A lossless CST / red-green tree (rust-analyzer, rowan).** The correct architecture for
  a human-facing editor: it retains trivia, supports semantic tokens, and recovers
  naturally. It also means rewriting mutsu's parser, and its two headline benefits —
  semantic tokens and caret-accurate completion — are both out of scope under D3.
- **A bespoke CLI or MCP tool instead of LSP.** Better fit for the consumer, worse
  distribution. See D2.
- **A big-bang span retrofit across `src/ast.rs`.** Touches the parser's hot path and the
  bincode AST precompilation cache, for value that D3 and D6 show is not needed up front.
- **Building on the RakuAST layer (ADR-0011).** It is a reflection/model layer over the
  internal AST for user code, carries no source positions, and is not a compiler frontend.
  It does not shorten this work.
- **A separate repository for the whole thing.** Rejected for the server (D7); accepted for
  editor extensions only.
- **Making diagnostics conservative to avoid disagreeing with rakudo.** This would be right
  for a language server aimed at the Raku ecosystem at large, where a false positive against
  rakudo-valid code is fatal to trust. It is wrong here: the target runtime *is* mutsu, so a
  mutsu-specific disagreement is exactly the signal the user needs (D4).

## Phasing

| Slice | Content | Depends on |
| --- | --- | --- |
| **S0** | Long-lived-process viability probe (D8) | — |
| **S1** | Server skeleton, full-document reparse, diagnostics from the existing single-error path | S0 |
| **S2** | Enumerable built-in name tables → "mutsu does not support this" diagnostics (D4) | S1 |
| **S3** | Multiple diagnostics per document + error recovery (give `parse_program_partial` positions and errors) | S1 |
| **S4** | `documentSymbol` / `workspaceSymbol` / `definition` at line granularity | S1 |
| **S5** | `references` / `hover`; expression spans on the variants these require (D6) | S4 |

S2 delivers the capability unique to mutsu and depends on no span work, so the ordering
front-loads distinctive value ahead of the heaviest engineering.

## Consequences

- mutsu gains a second, non-executing consumer of its parser. Parser changes acquire a new
  compatibility surface, enforced by in-tree CI (D7).
- The repository becomes a Cargo workspace.
- Positional correctness needs automated pinning from S1 onward (D5), because the intended
  consumer will not report it.
- Dependency symbol resolution remains the open scaling question: parsing a file correctly
  requires its dependencies' exported names, and `module_exports.rs` harvests those
  statically today only for the cases its scanner covers, with a documented fallback path.
  How far that generalizes across the ecosystem is unmeasured.

## Not decided here

- Whether the server ever grows the human-facing methods excluded by D3. Nothing in this
  design forecloses them; they are simply not paid for now.
- The precise mechanism for making built-in names enumerable (macro vs. asserted list).
- Whether the coverage database of D4 extends beyond built-in method and routine names to
  syntactic constructs mutsu does not yet parse.
- Transport and multi-root workspace handling.
- How mutsu's parser tracks rakudo's RakuAST-based grammar, which becomes rakudo's default
  in 2026.09 and replaces the legacy grammar in 6.e. This affects mutsu as a whole, not
  just the language server, and belongs in its own decision.
