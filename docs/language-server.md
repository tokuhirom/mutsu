# The mutsu language server

`mutsu-lsp` is a language server **for mutsu**: it reports what *mutsu* makes of
a Raku document. Its design, and the reasoning behind every scoping decision
below, is [ADR-0065](adr/0065-language-server-targets-ai-agents.md). The short
version: the primary consumer is an AI agent writing Raku that is meant to run
on mutsu, and that consumer decides which parts of the protocol are worth
implementing.

## Building and running

```
cargo build -p mutsu-lsp                  # debug
cargo build --release -p mutsu-lsp        # release
target/debug/mutsu-lsp --stdio
```

The repository is a Cargo workspace with two members: `mutsu` (the root package)
and `crates/mutsu-lsp`. A bare `cargo build` / `cargo test` at the root still
means the `mutsu` package alone — `default-members` in the root `Cargo.toml`
keeps that unchanged — so the server is built and tested explicitly with `-p
mutsu-lsp`, which is what CI's "Language server crate" step does.

The transport is stdio; `--stdio` is accepted (most clients pass it) and is the
only transport. Everything the server has to say about itself goes to stderr,
because stdout is the protocol channel.

The server is not part of the release tarball yet, and is versioned
independently of the interpreter (`tag-release.yml` bumps only the root
`Cargo.toml`). Shipping it is a decision for when it is worth shipping.

## What it does

| Method | Status |
| --- | --- |
| `initialize` / `shutdown` / `exit` | Yes |
| `textDocument/didOpen`, `didChange`, `didClose` | Yes, **full-document sync** |
| `textDocument/publishDiagnostics` | Yes — parse errors and parse warnings |
| `textDocument/documentSymbol` | Yes — nested, with the Raku declarator in `detail` |
| `workspace/symbol` | Yes — reads the workspace on demand, case-insensitive substring match |
| `textDocument/definition` | Yes — the open document first, then the workspace |
| `references`, `hover` | Planned (ADR-0065 S5) |
| `completion`, `semanticTokens`, `signatureHelp`, `inlayHint` | **Out of scope** (D3) |
| Incremental document sync | **Out of scope** (D3) |

The exclusions are structural, not cosmetic. Dropping `semanticTokens` removes
the need to write a lexer, which mutsu does not have. Dropping `completion`
removes caret-position scope resolution, the hardest form of positional
analysis. Dropping incremental sync deletes the document-diffing subsystem
outright — a full re-parse costs about 1.3 ms in process, measured by the S0
probe (`tests/long_lived_parse.rs`).

A request for a method the server does not implement is answered with
`MethodNotFound` rather than ignored: a client waiting forever for a response it
will never get is a worse failure than one told plainly.

## What the diagnostics mean

Diagnostics carry `source: "mutsu"`, because the whole point is that this is
*mutsu's* verdict, not rakudo's. A construct rakudo accepts and mutsu does not
is a true positive here — an agent writing code for mutsu has no other way to
learn mutsu's coverage short of running it (D4).

Today the server reports:

- **Every parse failure in the document.** mutsu's strict parser stops at the
  first one, so the server reports that (its diagnosis is the richest available:
  typed `X::` message, source context, hint) and then re-parses with recovery to
  find what else is wrong. Recovered failures are rendered through the same code
  path, so they are the same quality — and deduplicated by line, because the
  recovering pass sees the first failure again and a repeat on an
  already-reported line is more likely to be debris than a second defect.
- **Parse warnings**, at line granularity — sink-context warnings, VCS conflict
  markers, and the rest of what the parser collects while reading a unit.
- **Calls to routines mutsu does not have** (`code: "UndeclaredRoutine"`), with
  the replacement mutsu already computes: `sub greeting() { }; greetng()`
  answers "Did you mean 'greeting'?". This is the D4 signal — a core routine
  rakudo has and mutsu lacks reports exactly as a typo does, which is the point.
  It is mutsu's own CHECK-time `X::Undeclared::Symbols` scan, and it inherits
  that scan's contract: declarations are collected scope-blind across the unit,
  and a unit that imports names the walker cannot see through is abandoned
  rather than second-guessed, so a missed construct is a false negative and
  never a false positive. No `Interpreter` is constructed for it.
- **A parser crash, as a diagnostic**, phrased as a mutsu bug rather than a
  syntax error. `mutsu::analysis::check` catches the panic; a resident server
  cannot die on one bad document.

Unknown *method* names are not reported. `$x.foo` cannot be judged without
knowing what `$x` is, and mutsu's AST carries no type information — the same
reason it carries no positions. The existing `(owner, name)` catalog
(`src/builtins/native_method_row.rs`) cannot stand in for that: it is
deliberately conservative, so absence from it means "unclassified", not "mutsu
does not have it", and reporting absence as a defect would be a false positive.
See the ADR's S2 findings.

## Symbols and definitions

`documentSymbol` returns a nested outline: classes, roles, grammars, modules,
subs, methods, tokens, rules, enums (with their values), subsets, attributes and
top-level variables. A local inside a routine body is not an outline entry — it
would bury the outline in noise — but the same declaration at the top of a class
or a file is.

LSP's `SymbolKind` has no spelling for a role, a grammar, a grammar token or a
subset, so the mapping picks the nearest behavioural equivalent and puts the real
Raku declarator in `detail`. An outline entry that says `CLASS` still reads
"grammar".

Ranges are line-granular, as everything here is: a declaration runs from its line
to the last line its body demonstrably covers (the deepest `SetLine` marker
inside it), which stops at its last statement rather than at the closing brace.
`selectionRange` is exact, though — the name is a literal and the declaration
line is short, so it is found in the text.

`definition` needs no AST positions in either direction. The target is a
declaration, which `SetLine` places; the source is whatever identifier the caret
is on, which the server reads straight out of the document text. It searches the
open document first (both the likeliest answer and the freshest, since the copy
on disk may be older than what the client holds), then the workspace.

`workspace/symbol` walks the workspace roots, parses what it finds and caches by
modification time and size. There is no background index: an agent asks a
workspace question occasionally and never while typing, and validating the cache
against the file removes a class of staleness bug. The walk is capped at 4000
files, because a query over an unbounded tree is a hang and a hung server is
worse than a truncated answer.

A parse failure inside a `use`d module is anchored at line 1 of the *open*
document and names the other file in its message. Reporting that module's
line and column against this document would point at an unrelated line, which
under D5 is worse than pointing at nothing.

## Nothing is executed

`mutsu::analysis::check` parses and throws the AST away. It never compiles and
never runs, so opening a document with `unlink "/etc/passwd"` in it is safe.
Parsing a `use` *does* read the imported module's source off disk — Raku's
grammar is not context-free with respect to the imported symbol table, so the
exported names have to be harvested to parse the importer correctly — but it
does not run that module either.

## Positions

LSP positions are 0-based with **UTF-16 code unit** columns; mutsu's are 1-based
with character columns. Every diagnostic passes through
`crates/mutsu-lsp/src/positions.rs`, and that conversion is pinned by tests
rather than checked by looking at an editor.

That is not fussiness. An agent tolerates a range that is off by a few
characters — it re-reads the line — and therefore never reports one. A human
would notice the squiggle in the wrong place; the intended consumer will absorb
it silently while the quality of the server rots unobserved. So positional
assertions are mandatory from the first slice (D5), including at least one
document with astral-plane characters, where a character count and a UTF-16
offset disagree.

mutsu reports a *point*, not a span: the AST carries no positions at all (D6),
and a parse failure carries only the offset it ejected at. A diagnostic's range
therefore runs from that point to the end of its line — for a parse failure the
remainder of the line is precisely what mutsu could not make sense of. A warning,
which has no column, covers the whole line.

## Layout

| File | What lives there |
| --- | --- |
| `src/analysis.rs` (in `mutsu`) | The non-executing frontend: `check(source) -> Vec<Diagnostic>`. The only entry point in the interpreter that parses a document and keeps nothing but what it learned. |
| `src/runtime/undeclared_routines.rs` (in `mutsu`) | The CHECK-time undeclared-routine walker, shared by the interpreter and the frontend. `check_undeclared_routines_without_interpreter` is the frontend's entry point; the static name predicates live in one function so the two paths cannot drift. |
| `crates/mutsu-lsp/src/server.rs` | The protocol loop: capabilities, document sync, diagnostic publication, shutdown. |
| `crates/mutsu-lsp/src/positions.rs` | mutsu positions to LSP positions. |
| `crates/mutsu-lsp/src/diagnostics.rs` | `mutsu::analysis::Diagnostic` to `lsp_types::Diagnostic`. |
| `crates/mutsu-lsp/src/documents.rs` | The open-document map. No rope, no diffing — sync is full-text. |
| `src/analysis/symbols.rs` (in `mutsu`) | The declarations a document contains, nested, at line granularity. Runs over a recovering parse, so a broken document still has an outline. |
| `crates/mutsu-lsp/src/symbols.rs` | mutsu's declarations to LSP symbols, and the `SymbolKind` mapping. |
| `crates/mutsu-lsp/src/workspace.rs` | The files a workspace-wide query reads, and their cache. |
| `crates/mutsu-lsp/tests/protocol.rs` | End-to-end tests over `lsp_server::Connection::memory()`, driving the real loop. |

The loop is synchronous and single-threaded on purpose: D3 leaves nothing
latency-sensitive to overlap, and parsing on the loop thread keeps the parser's
thread-local caches warm — the exact configuration the S0 probe validated for a
long-lived process.

It is **not** the OS main thread, though. mutsu's parser is deeply recursive
enough that on a default 8 MB stack it overflows at about fifty nested
parentheses, and a stack overflow aborts the process — `catch_unwind` cannot turn
that into a diagnostic the way it does an ordinary panic. The interpreter's own
CLI spawns a 256 MB-stack thread for the same reason; the server runs its loop
inside `mutsu_lsp::on_analysis_stack`. Any new front end for mutsu's parser
inherits this requirement: it is a property of the parser, not of the CLI. `lsp-server` + `lsp-types` (rust-analyzer's crates) rather
than `tower-lsp`, so no async runtime enters this repository at all.

## Why it lives in this repository

The server has to track mutsu's parser in lock-step. In a separate repository it
would break silently on a parser change; in-tree, CI catches it (D7). Editor
extensions are the opposite case — different language, different registry,
different cadence — and belong in their own repository.
