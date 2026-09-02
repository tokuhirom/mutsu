# mutsu has a language server, and it tells you what *mutsu* thinks of your code

ADR-0065 S1 has landed: `mutsu-lsp` speaks LSP over stdio, holds documents open,
re-parses them in full on every change, and publishes diagnostics. The
repository is now a Cargo workspace, and mutsu itself has grown a public
non-executing analysis frontend.

The point is not that mutsu has an editor integration. It is that a diagnostic
saying "mutsu cannot do this" is now something you can get without running the
code. An agent writing Raku targeted at mutsu previously had exactly one way to
discover mutsu's coverage: execute and see what broke.

## `mutsu::analysis::check` — the one entry point that parses and keeps nothing

Every other place in the crate that parses Raku does so on the way to running
it. `src/analysis.rs` parses a document, throws the AST away and reports what it
learned: parse errors with line and column, parse warnings at line granularity,
and — importantly — a parser *crash*, phrased as a mutsu bug rather than as a
syntax error in your code. A resident server outlives any one document, so a
panic on malformed input has to become a diagnostic, not an abort.

Nothing executes. Opening a file with `unlink "/etc/passwd"` in it is safe.
Parsing a `use` does read the imported module's source off disk — Raku's grammar
is not context-free with respect to the imported symbol table, so the exported
names have to be harvested before the importer can be parsed correctly — but it
does not run that module either.

Warnings turned out to be free: the parser already collects them, tagged with a
`"\n    at FILE:LINE"` suffix baked into the message text (it has to survive the
precompilation cache, which persists text only). Splitting that suffix back off
recovers the line, so sink-context warnings and VCS conflict markers reach the
editor already.

## The scope is set by who reads it

ADR-0065 D3 implements only the methods an AI agent consumes, and the exclusions
are structural rather than cosmetic. Dropping `semanticTokens` removes the need
to write a lexer, which mutsu does not have. Dropping `completion` removes
caret-position scope resolution, the hardest form of positional analysis.
Dropping incremental document sync deletes the document-diffing subsystem
outright — the S0 probe measured a full in-process re-parse at about 1.3 ms, so
there is nothing to optimize.

What is left is small enough to be synchronous and single-threaded.
`lsp-server` + `lsp-types` — rust-analyzer's crates, a thread-and-channels loop
with no runtime — rather than `tower-lsp`, so no async runtime enters this
repository at all. Parsing happens on the loop thread, which also keeps the
parser's thread-local caches warm: exactly the configuration S0 validated.

A request for an unimplemented method gets `MethodNotFound` rather than silence.
A client waiting forever for a response it will never receive is a worse failure
than one told plainly.

## Positions are pinned by tests, because the consumer will never complain

LSP counts columns in UTF-16 code units from 0; mutsu counts characters from 1.
Every diagnostic passes through one conversion, and that conversion is asserted
in tests rather than eyeballed in an editor — including a document with
astral-plane characters, where the two disagree. mutsu reports the stray `}` in
`my $t = '🐪🐪🐪🐪' }` at character column 16; LSP has to be told 19.

This is D5's hazard made concrete. A human notices a squiggle in the wrong
place. An agent re-reads the line, absorbs the imprecision, and never reports
it — so the server's quality can rot completely unobserved. Positional
assertions are therefore mandatory from the first slice, not a later polish.

Since mutsu reports a *point* rather than a span (the AST has no positions at
all, D6), a diagnostic's range runs from that point to the end of its line: for a
parse failure the remainder of the line is precisely what mutsu could not make
sense of. A warning, which carries no column, covers the whole line.

## The repository is a workspace now

`mutsu` stays the root package and `crates/mutsu-lsp` joins it as a member, with
`default-members = ["."]` so a bare `cargo build` or `cargo test` at the root
means exactly what it meant before. The server is built and tested explicitly
(`-p mutsu-lsp`), which CI does as its own step and `make test` now mirrors.

It lives in-tree because it has to track mutsu's parser in lock-step: in a
separate repository it would break silently on a parser change (D7). It takes
mutsu with default features, JIT included — a leaner feature set would be a
different build of the whole interpreter, making CI compile mutsu a third time
for a binary that is merely smaller.

The end-to-end tests drive the real loop over `lsp_server::Connection::memory()`,
so the initialize handshake, document sync, diagnostic withdrawal on close, and
shutdown are all exercised as a client sees them — including a document sequence
designed to break the parser, after which the server must still be answering.

Usage and layout: `docs/language-server.md`. Next up is S2, the capability that
is unique to mutsu: making the built-in name tables enumerable so "mutsu does not
implement this method" becomes a first-class diagnostic (D4).
