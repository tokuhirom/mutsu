# `references` (ADR-0065 S5b) probably wants a parser side table, not spans on AST variants

ADR-0065 D6 assumed `references` would be paid for by adding spans to the AST
variants it needs — "declaration nodes first (~10), reference nodes second
(~5)". Reconnaissance while building S5a suggests that is the expensive way to
do it, and possibly the wrong shape entirely.

## What `references` actually needs

Not a tree. A **list of occurrences with positions**: for each place a name is
used as a reference, its byte offset and what kind of reference it is
(variable, routine call, method call, type). The AST is a structure for
*executing* code; a reference index is analysis metadata that happens to be
discoverable at the same time.

## The parser already knows the offset, with no AST change

Position derivation is pointer arithmetic against a thread-local base:
`current_line_number` (`src/parser/primary/mod.rs:110`) computes a slice's
position as `input.as_ptr() - ORIGINAL_SOURCE.ptr`, and
`is_within_original_source` (line 218) validates that a slice belongs to the
buffer at all. A `current_offset(input) -> Option<usize>` helper is a few lines.

So at every site where the parser builds `Expr::Var`, `Expr::FuncCall`,
`Expr::MethodCall` or a bareword type reference, the byte offset is already in
hand. Pushing `(offset, name, kind)` into a thread-local `Vec` — gated on an
analysis-mode flag so an ordinary parse pays one bool check and no allocation —
would produce the index directly.

This avoids all three costs D6 was worried about:

- no change to `Expr`'s variants, so no change to its size or to the parser's
  hot path;
- no change to the bincode AST precompilation cache's serialized shape;
- no retrofit spreading across construction sites in the parser tree (the push
  is *at* those sites, but it is additive and local).

## The hazard that has to be measured first: backtracking

**The parser backtracks.** A `Var` parsed inside an alternative that later fails
would be recorded even though the successful parse never treated that text as a
variable. Recording naively would manufacture phantom references, and under D5 a
plausible-looking wrong answer is the expensive kind of mistake.

Two partial mitigations, neither obviously sufficient:

1. **Deduplicate by `(offset, name, kind)`.** A backtracked parse and the
   successful one produce the same tuple for the same text, so re-parses
   collapse. This does *not* filter an offset the successful parse read
   differently (as part of a string, say).
2. **Verify each recorded offset against the source text** — the bytes at that
   offset must still spell the name. Cheap and exact, but it only catches
   offsets that drifted, not offsets that were reinterpreted.

The sound alternative is a checkpoint/rollback discipline around every
alternation point, which the parser has thousands of; that is invasive enough to
change the verdict on this whole approach.

**So the first step is measurement, not implementation**: build the side table
behind the analysis flag, run it over `modules/`, `vendor/` and `t/`, and compare
the recorded occurrences against a naive text scan for the same identifier. The
occurrences a text scan finds and the parser does not are the win (comments,
strings). The ones the parser records that sit inside a comment or a string are
the phantom rate, and that number decides whether dedup + text-verify is enough.

## The other open question: scope

Even with a perfect occurrence list, `references` for `$x` returns every
occurrence of that *name*, not every reference to that *declaration*. mutsu's
parser discards its lexical scope when parsing ends (`SCOPES`, noted in the
ADR's "What mutsu does not have").

That may be acceptable and should be stated rather than hidden: name-based
references, excluding comments and strings, is already strictly better than
grep, and D5 demands that the server not claim more precision than it has. But
it is a decision, not an oversight, and belongs in the ADR before the method
ships.

## Why this is `todo/deep/` and not a ticket

It changes what S5b *is* relative to what the ADR says, it needs a measurement
before the design can be settled, and it touches the parser at many sites. The
next session should start from the measurement above, then amend ADR-0065's D6
and S5b entry with what it finds.
