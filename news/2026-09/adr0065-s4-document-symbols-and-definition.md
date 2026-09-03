# Symbols and go-to-definition, without adding a single position to the AST

ADR-0065 S4 lands `documentSymbol`, `workspace/symbol` and `definition` —
the methods D3 kept because they give an agent an exact answer where it would
otherwise grep, and grep's false positives (a mention in a comment, a string, an
unrelated same-named method) are what make it a poor substitute.

D6's bet was that line granularity would carry them with no span retrofit across
`src/ast.rs`'s ~80 variants. It did, and two of the three needed less positional
machinery than the design expected.

## `SetLine` carries the outline, and `definition` needs no positions at all

The parser interleaves a `Stmt::SetLine` marker before every statement, including
inside a class or routine body. Walking the statement list while tracking the
most recent marker yields each declaration's line for free. A declaration's *end*
is approximated by the deepest marker inside its body, so it stops at its last
statement rather than at the closing brace — accurate enough for an outline, and
honest about what the AST actually knows.

`definition` turned out to need AST positions in neither direction. The **target**
is a declaration, which `SetLine` places. The **source** is whatever identifier
the caret is on — and the server has the document text, where an identifier is a
lexical notion that needs no parse at all. Reading it straight out of the line
sidesteps the "no positions for references" problem entirely.

That does not generalize, and it is worth being clear about why: `references`
(S5) must find *every* occurrence and rank them, which text scanning cannot do
soundly. `definition` gets away with it because it only needs to know what one
word is.

`selectionRange` is exact rather than line-wide for the same reason. The name is
a literal and the declaration line is short, so it is found in the text — which
is where a client puts the caret on "go to symbol". The match is anchored at
identifier boundaries, or `has $.x` would select the `x` inside a nearby `max`.

## The best-effort parser was not emitting the markers

S3 was ordered before S4 precisely so the outline would survive a document under
edit. It did not: `stmt_list_partial` never emitted `SetLine` — only the strict
statement list did — so a broken document's outline reported every declaration on
line 1. It now emits them exactly as the strict list does. Consumers of a
best-effort parse are unaffected, since they match on declaration variants and
ignore markers, which is already what they do for a strict parse.

## LSP's vocabulary has no Raku in it, so the declarator goes in `detail`

There is no `SymbolKind` for a role, a grammar, a grammar token or a subset. The
mapping picks the nearest behavioural equivalent — a role is an interface, a
token is a function, a grammar is a class — and puts the real Raku declarator in
`detail`, so an outline entry that says `CLASS` still reads "grammar". Dropping
that would be a quiet downgrade of exactly the information a Raku reader is
scanning an outline for.

## Workspace queries read on demand instead of maintaining an index

`workspace/symbol` and a cross-file `definition` walk the roots, parse what they
find, and cache by modification time and size. Nothing runs in the background.
That is the right trade for this consumer — an agent asks a workspace question
occasionally and never while typing — and it removes a class of staleness bug,
because the cache is validated against the file rather than trusted. The walk is
capped at 4000 files: a query over an unbounded tree is a hang, and a hung server
is worse to the consumer than a truncated answer.

`rootUri` and `rootPath` are read alongside `workspaceFolders`. They are
deprecated, but clients still send them, and a server that understood only the
current spelling would silently have no workspace — a failure that presents as
"no results" rather than as a bug.
