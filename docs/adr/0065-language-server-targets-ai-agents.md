# ADR-0065: mutsu's language server targets AI agents, and is scoped to the protocol surface an agent actually consumes

- **Status**: Accepted (2026-09-02); S0 and S1 shipped 2026-09-03. See the phasing table
  and the S0/S1 findings sections below for what has actually been built.
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

**Executed 2026-09-03 — the gate passes.** See "S0 findings" below. Two of this
paragraph's expectations were wrong, and the findings section carries the corrections:
concurrent parsing of different documents *does* hold, and byte-identical re-parse is not
an achievable (or desirable) property.

## S0 findings (2026-09-03)

The probe is `tests/long_lived_parse.rs`, five tests that run as part of `cargo test` and
stay in the suite as regression gates. Iteration count is `MUTSU_S0_ITERATIONS` (200 by
default, so the committed gate is cheap); the numbers below are from a debug build at 8000
iterations, which is what a resident server reaches in a working session. **These are
feasibility measurements, not bench-CI numbers** — memory and determinism results are
independent of optimization level, and the wall-clock figures are not the ones to quote.

**The gate passes.** Nothing here invalidates the plan. Five results, two of which correct
D8 as written above.

### 1. Re-parsing is deterministic, but not byte-identical — and must not be

An unchanged document re-parses to an identical AST **except** for ids the parser is
*required* to mint uniquely per declaration site:

- `decl_id` — a `my class`'s key in the global type registry. ADR-0047 D1 mangles every
  lexical declaration to `Foo\u{0}<decl-id>` precisely so that two declaration sites can
  never share a registry key.
- `__ANON_CLASS_N__`, `__ANON_ROLE_N__`, `__ANON_SUBSET_N__` — registry *names* for
  anonymous declarations, drawn from the same process-global counters that the runtime's
  `but`-mixin path also draws from (`next_anon_role_name`).
- Desugaring temporaries: `__with_tmp_N`, `__if_bind_tmp_N`, `__take_value_N`, ... These
  name lexicals inside a desugared block rather than registry entries, so unlike the two
  above they are *not* known to require process-global uniqueness — but nothing has
  established that they don't, and they drift the same way.

For the registry ids, resetting per parse would make two declaration sites in two different
compilation units collide in a process-global table — a correctness bug, not a cleanup. So
the right gate is *determinism modulo those ids*, and the probe normalizes them before
comparing. With that normalization, 8000 consecutive re-parses of an 1140-byte document
are identical. Any other difference is residual parser state and fails the test.

### 2. The only unbounded growth is one interned name per anonymous declaration per parse

`src/symbol.rs` leaks interned strings for the process lifetime by design. Measured over
8000 re-parses:

| Document | Interned names | Resident memory |
| --- | --- | --- |
| No anonymous declarations | **+0** (exactly zero) | +124 KiB at 8000 parses, +136 KiB at 2000 — noise, not growth |
| One anonymous class | +8000 (exactly 1.00/parse) | +3988 KiB (~0.5 KiB/parse, linear) |

So there is no general per-parse leak: the parse memo tables reset and genuinely release,
and `Vec`/`String` churn returns to the allocator. The *entire* linear component is the
freshly minted, interned, permanently leaked registry name for each anonymous declaration.
At ~0.5 KiB per re-parse of a file containing one `class { }` this is a slow leak — a few
megabytes over a long editing session — not a blocker.

The structural fix is available and belongs with the server's real entry point, not here:
**an analysis-only parse never registers a type**, so in that mode these counters can be
compilation-unit-local instead of process-global. That is a property of the API S1
introduces, so it is recorded as a follow-up
(`todo/tickets/analysis-parse-mints-process-unique-registry-names.md`) rather than
retrofitted onto `dump_ast`.

### 3. Concurrent parsing of different documents holds — D8's expectation was wrong

The parser's entire working set is thread-local (`SCOPES`, the three memo tables,
`ORIGINAL_SOURCE`, `LEAKED_REGIONS`, the slang modes); the symbol table is behind an
`RwLock` and the unique-id counters are atomics. Four threads parsing four different
documents, five rounds, produce ASTs identical to the same documents parsed on the main
thread. **The server is therefore not forced to serialize parsing.** This is scoped to
parsing only — it says nothing about concurrently *loading modules* or executing, which
touch the type registry and the interpreter's globals.

### 4. No residue between documents, and `ORIGINAL_SOURCE` survives re-entry

Parsing a document that declares custom infix/prefix operators, a `use v6.e.PREVIEW`
pragma, lexicals, a `constant` and a `my class` does not change how the *next* document
parses (checked in both orders — a B/A/B and an A/B/A sandwich). This is the failure mode
that a one-shot process can never expose and that would make a resident server's
diagnostics depend on which file was opened first.

### 5. In-process re-parse costs ~1.3 ms, which re-confirms D3's rejection of incremental sync

The ADR's feasibility table records ~9 ms as the average parse of one ecosystem module,
measured through the CLI — one process per file, so it includes startup and I/O. Measured
*in process*, which is what a server actually does, a release build re-parses the 1140-byte
probe document in **1.29 ms** (8000 iterations, 10.3 s total). Debug is ~10.7 ms.

D3 dropped incremental document sync on the argument that a full reparse is fast enough.
The in-process figure is roughly seven times better than the number that argument was made
on, so the decision holds with a wide margin. Memory and determinism results are identical
between debug and release, as expected.

`ORIGINAL_SOURCE`'s raw `(pointer, len)` pair also survives interleaving parses of
differently sized buffers, including buffers that trigger nested sub-parses on separate
allocations (a heredoc and an `EVAL`). Line numbers in the small document stay correct
after 25 rounds of being interleaved with a 200-line buffer, so the existing
snapshot/restore discipline in `parse_program_partial` holds under repetition. Since
`Stmt::SetLine` is the only positional information mutsu has (D6), this is the load-bearing
property for every diagnostic the server will emit.

## S1 findings (2026-09-03)

Shipped: `crates/mutsu-lsp` (the server), `src/analysis.rs` (mutsu's non-executing
frontend), `docs/language-server.md` (usage and layout). The repository is now a Cargo
workspace. Four things are worth recording because they either settled a "not decided
here" item or came out differently from the design.

### 1. `lsp-server` + `lsp-types`, not `tower-lsp` — D3 removed the need for async

D7 anticipated keeping `tower-lsp`/`tokio` out of mutsu's dependency tree by putting the
server in its own crate. With D3 in hand that turns out to be unnecessary: dropping every
latency-sensitive method leaves nothing to overlap, so the server is a synchronous
thread-and-channels loop over rust-analyzer's `lsp-server`, and **no async runtime enters
this repository at all**. Parsing runs on the loop thread, which also keeps the parser's
thread-local caches warm — the exact configuration S0 validated.

### 2. Warnings were free, and they carry their own line

The design assumed S1 would ship the single parse error and that everything else waited for
S3. In fact `PARSE_WARNINGS` already collects the parser's warnings with a
`"\n    at FILE:LINE"` suffix baked into the message text (it has to survive the
precompilation cache, which persists text only). Splitting that suffix back off recovers a
line number, so sink-context warnings and VCS conflict markers ship in S1 at line
granularity. S3 remains about *multiple errors* and recovery, which is the hard part.

### 3. A parser panic is a diagnostic, not an abort

Not in the design, and load-bearing for a resident process: mutsu's parser is not
panic-free, and a server must outlive a document that trips it. `analysis::check` catches
the unwind and reports it as "mutsu's parser crashed on this document ... this is a bug in
mutsu", which is D4's signal in its bluntest form — an agent must not go looking for a
mistake in its own code. Pinned by a protocol test that feeds a sequence of hostile
documents and then asserts the session is still analysing.

### 4. The workspace split cost less than expected

`mutsu` stays the root package with `crates/mutsu-lsp` as a member and
`default-members = ["."]`, so a bare `cargo build` / `cargo test` at the root means exactly
what it meant before, and every existing CI, release, container and wasm invocation is
untouched. The server needs one CI step of its own (`cargo clippy -p mutsu-lsp
--all-targets` + `cargo test -p mutsu-lsp`), mirrored in `make test`.

It takes mutsu with **default** features, JIT included. A leaner feature set is a different
build of the whole interpreter, which would make CI compile mutsu a third time (it already
builds debug and release) for a binary that is merely smaller.

Two items from "Not decided here" are now decided in passing: `mutsu-lsp` is versioned
independently of the interpreter (`tag-release.yml` bumps only the root `Cargo.toml`) and
is **not** in the release tarball yet; transport is stdio only.

## S2 findings (2026-09-03)

D4 said the work here was to make mutsu's built-in names enumerable: "they are
currently string literals in `match method { ... }` arms ... the fix is to derive the
dispatch arms and a name table from one source". Both halves of that turned out to be
wrong, in opposite directions.

### 1. The table already exists — and is the wrong shape for a diagnostic

`src/builtins/native_method_row.rs` (ADR-0019 Phase E box E2a) is an `(owner, name)`
catalog with per-arity recognition flags, already read in production by `.^methods` and
`.^can`. So no enumeration work was needed.

It cannot back a diagnostic, though, because it is **deliberately conservative in the
direction that produces false positives**. A pair with no row reports "not servable", and
whole owners are uncovered by construction — `Sub`, `Signature`, `IO::Path`, `IO::Handle`,
`Cool`, and the untouched majority of `Any`/`Mu`'s surface. Absence from the table means
"the 2026-08-10 probe did not classify this", not "mutsu does not have it". Reporting
absence as a defect would tell an agent that a method mutsu implements does not exist,
which is precisely the failure D5 says is unrecoverable.

### 2. The real blocker for method diagnostics is the receiver type, not the name list

`$x.foo` cannot be judged without knowing what `$x` is, and mutsu's AST carries no type
information for the same reason it carries no positions. D4 did not account for this. The
honest scope for method-name diagnostics is therefore the subset where the receiver is
statically known — a literal, or a bareword type object (`Int.frobnicate`) — plus a table
that distinguishes "known absent" from "unclassified". That is a separate slice with a
real design question in it, and it is not what shipped here.

### 3. The routine half needs no receiver, and mutsu already had it

A call with no receiver has no ambiguity, so D4's signal is available immediately there:
a core routine rakudo has and mutsu lacks reports exactly as a typo does, which is the
point. And `src/runtime/undeclared_routines.rs` already implements rakudo's CHECK-time
`X::Undeclared::Symbols` scan, with the contract a diagnostic needs stated in its own
module docs: declarations are collected scope-blind across the unit and the check
abandons a unit that imports names it cannot see through, so *"a missed construct can
only produce a false negative, never a false positive"*.

S2 therefore ships the routine half by wiring that existing analysis into
`analysis::check`, and leaves the method half to a later slice.

### 4. The analysis path constructs no `Interpreter`, and that is load-bearing

The obvious implementation — build an `Interpreter` and call the runtime's own
`check_undeclared_routines_mainline` — measured at **9.2 ms and ~7.2 KiB retained per
construction** (debug, 4000 constructions, linear, unaffected by `MUTSU_GC=on`). On the
same build that is twice the cost of parsing the whole document and fifteen times its
memory, paid on every keystroke.

Since every lookup the runtime path adds is per-interpreter registry state that a *fresh*
interpreter has none of, the verdict is identical without one. The static predicates were
factored into a single shared function so the two paths cannot drift, and
`check_undeclared_routines_without_interpreter` is what the frontend calls. Analysis is
now 5.0 ms per document with 0.52 KiB/call retained — the same memory profile as a plain
parse, and cheaper than `dump_ast`, which additionally formats the AST.

The interpreter-construction cost is recorded separately
(`todo/perf/interpreter-new-is-expensive-and-retains-memory.md`): it is not an
LSP-specific problem.

### 5. D4's "carry the replacement" exposed a rakudo-parity gap in mutsu itself

mutsu suggested a replacement for a core routine (`elem` → `elems`) but never for the
unit's own: `sub greeting() { }; greetng()` reported the typo with no way to see what was
meant, where rakudo answers "Did you mean 'greeting'?". Its suggestion candidates came
from the interpreter's registry, which does not hold the unit's declarations at the point
the check runs — while the walker had already collected them.

The walker now tracks routine declarations separately from the names it collects to
*suppress* calls. That distinction matters: the suppressing set deliberately absorbs
variables and types, so drawing suggestions from it would offer a `my $greeting` as the
routine you meant, which rakudo never does. Pinned by
`t/undeclared-routine-suggests-unit-own-subs.t`, which passes unmodified under real raku.

This is the D7 property in practice — the language server's requirements improving mutsu's
own diagnostics rather than taxing them.

## S3 findings (2026-09-03)

### 1. The slice is better understood as "keep analysing past the first failure"

Framed as "multiple diagnostics per document" this looks like a nicety: a document usually
has one syntax error. The framing that matters is the other one — **a document under edit
is broken most of the time**, and a report that goes quiet after the first failure hides
everything below it. That also makes S3 a prerequisite for S4 being useful at all: symbols
and definitions are wanted *while* the file is mid-edit, not only when it is complete.

### 2. Recovery reuses the strict parser's diagnosis rather than a lower tier

`parse_program`'s ~110-line failure-rendering block was extracted as `render_parse_error`,
so a skipped statement is diagnosed to exactly the same standard as the first failure —
typed `X::` message where an alternative diagnosed one, rakudo's `.pre`/`.post` context,
the hint. Under D5 that matters more than the count: a second diagnostic of lower quality
would be worse than no second diagnostic.

This needed no offset arithmetic. A `PError`'s `remaining_len` measures the *shared
buffer's* unconsumed tail, not an offset within whichever suffix the failing parser was
called on, so a failure raised inside `statement(rest)` already locates itself in the whole
source.

### 3. Cascade risk was measured, not assumed, and is low

Over the 217 files of `modules/`, 11 fail to parse, **2 report more than one failure**, for
11 extra diagnostics. Inspecting those two: every extra points at a distinct real
construct on its own line, not at debris from the previous skip. Recovery is deduplicated
by line against what is already reported — the recovering pass re-parses from scratch, so
its first failure is the strict parse's failure seen again, and a second failure on a line
already accounted for is far more likely to be a cascade than a second defect. The tie is
broken toward saying less.

The undeclared-routine analysis deliberately does **not** run on a recovered parse. Its
false-positive direction inverts there: a skipped statement may have held the very `sub`
declaration that explains a later call. `stmt_list_partial`'s existing
`note_partial_parse_skip` exists for exactly this class of consumer.

### 4. The server was one deep document away from dying, and S1 shipped it that way

The survey overflowed its stack before it produced a single number, which exposed a defect
in S1: `mutsu-lsp` parsed on the OS main thread. mutsu's own CLI does not — `src/main.rs`
spawns a 256 MB-stack thread because grammar matching and nested expression parsing are
deeply recursive.

Measured on a debug build: with an 8 MB stack, `my $x = ((( ... )))` **overflows at about
fifty nested parentheses**; twenty are fine. With the analysis stack, a thousand are fine.
Fifty is not exotic — a nested data literal reaches it.

A stack overflow **aborts the process**. `analysis::check`'s `catch_unwind`, which turns a
parser panic into a diagnostic (S1 finding 3), cannot rescue it, so the whole session would
have died — every open document with it — on a file the CLI reads without complaint. The
server now runs its loop inside `mutsu_lsp::on_analysis_stack`, and the protocol tests
spawn their server the same way, so a regression aborts the test binary rather than passing
quietly.

The general lesson is worth stating: **any new front end for mutsu's parser inherits the
CLI's stack requirement.** It is not a property of the CLI; it is a property of the parser.

## S4 findings (2026-09-03)

D6's bet was that line granularity would carry `documentSymbol` and `definition` with no
span retrofit. It did, and two of the three methods needed less positional machinery than
expected.

### 1. `Stmt::SetLine` carries the outline on its own — and `definition` needs no positions at all

The parser interleaves a `SetLine` marker before every statement, including inside a class
or routine body, so walking the statement list while tracking the most recent marker yields
each declaration's line for free. A declaration's *end* is approximated by the deepest
marker inside its body, which stops at its last statement rather than at the closing brace
— accurate enough for an outline, and honest about what the AST knows.

`definition` turned out not to need AST positions in either direction. The *target* is a
declaration, which `SetLine` places. The *source* is whatever identifier the caret is on,
and the server has the document text — an identifier is a lexical notion that needs no
parse. Reading it out of the line sidesteps the whole "no positions for references"
problem that D6 flagged as `references`'s (S5's) real cost. Note that this does not
generalize: `references` needs to find *every* occurrence and rank them, which grep-like
text scanning cannot do soundly.

`selectionRange` is exact rather than line-wide for the same reason: the name is a literal
and the declaration line is short, so finding it in the text gives the range a client puts
the caret on. The match is anchored at identifier boundaries, or `has $.x` would select the
`x` inside a nearby `max`.

### 2. The best-effort parser was not emitting the markers, which S4 depended on

`stmt_list_partial` never emitted `SetLine`; only the strict list did. The outline of a
*broken* document — the whole point of ordering S3 before S4 — therefore reported every
declaration on line 1. It now emits them exactly as the strict list does. Consumers of a
best-effort parse are unaffected: they match on declaration variants and ignore markers,
which is already what they do for a strict parse.

### 3. LSP's `SymbolKind` has no Raku vocabulary, so the declarator goes in `detail`

There is no kind for a role, a grammar, a grammar token or a subset. The mapping picks the
nearest behavioural equivalent (a role is an interface, a token is a function, a grammar is
a class) and puts the real Raku declarator in `detail`, so an outline that says `CLASS`
still reads "grammar". Losing that distinction would be a quiet downgrade of exactly the
information a Raku reader is scanning for.

### 4. Workspace queries read on demand rather than maintaining an index

`workspaceSymbol` and a cross-file `definition` walk the roots, parse what they find, and
cache by modification time and size. No background index is maintained. That is the right
trade for this consumer — an agent asks a workspace question occasionally and never while
typing — and it removes a class of staleness bug, since the cache is validated against the
file rather than trusted. The walk is capped (4000 files): a query over an unbounded tree
is a hang, and a hung server is worse than a truncated answer.

`rootUri`/`rootPath` are read alongside `workspaceFolders`. They are deprecated, but
clients still send them, and a server that understood only the current spelling would
silently have no workspace at all — a failure that looks like "no results" rather than like
a bug.

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
| **S0** | Long-lived-process viability probe (D8) — **done 2026-09-03**, `tests/long_lived_parse.rs` | — |
| **S1** | Server skeleton, full-document reparse, diagnostics from the existing single-error path — **done 2026-09-03**, `crates/mutsu-lsp/`, `src/analysis.rs`, `docs/language-server.md` | S0 |
| **S2** | Enumerable built-in name tables → "mutsu does not support this" diagnostics (D4) — **routine half done 2026-09-03**; the method half is blocked on receiver types, see the S2 findings | S1 |
| **S3** | Multiple diagnostics per document + error recovery (give `parse_program_partial` positions and errors) — **done 2026-09-03** | S1 |
| **S4** | `documentSymbol` / `workspaceSymbol` / `definition` at line granularity — **done 2026-09-03** | S1 |
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
