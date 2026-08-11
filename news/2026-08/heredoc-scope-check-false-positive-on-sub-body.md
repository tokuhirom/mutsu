# Fixed: heredoc-in-sub false positive that blocked `CSV::Table`/`CSV-AutoClass` from loading

`check_heredoc_scope_errors` (added in #1430 to catch a real Raku gotcha —
a heredoc whose enclosing block closes before its own terminator can see a
`my` local go out of scope) was misfiring on the overwhelmingly common
pattern "`my $x = ...;` followed later by a `qq:to/…/` heredoc referencing
`$x`, inside the same `sub`":

```
$ mutsu -e 'sub foo() { my $x = "hi"; print qq:to/HERE/;
value: $x
HERE
}
foo();'
Variable '$x' is not declared. Perhaps you forgot a 'sub' if this was intended to be part of a signature?
```

Found while surveying CSV libraries on the ecosystem
([docs/batteries/csv.md](../../docs/batteries/csv.md)) — `CSV::Table`'s
dependency `Text::Utils::Subs.rakumod` and `CSV-AutoClass`'s own
`lib/CSV-AutoClass.rakumod` both hit this exact shape and failed to even
`use`.

## Root cause

Raku's heredoc parsing genuinely does have the gotcha the check was written
for: the body/terminator text physically following a `qq:to/DELIM/` marker
is spliced onto whatever comes after — so if a `}` closing the declaring
block sits on the marker's OWN source line (e.g.
`sub f() { my $a = 'foo'; qq:to/END/ }`, with the body/terminator on
subsequent lines), that block has already closed by the time the heredoc's
content is resolved, and a `my` local from it is correctly out of scope
(confirmed against `roast/S02-literals/heredocs.t`'s own "heredoc fails in
block 2a"/"block 4" cases). But when the marker is an ordinary statement
(ends in `;`, nothing else on that line) — the vastly more common style,
used by real-world modules like `Text::Utils` — the declaring block stays
open through the whole heredoc and normal lexical scoping applies; the old
check had no way to tell the two cases apart and flagged both.

## Fix

`Expr::HeredocInterpolation` now carries a second field, computed by the
parser at `src/parser/primary/string/heredoc.rs`: whether the raw text
remaining on the heredoc marker's own physical line (before its
body/terminator get spliced in) contains a `}` — i.e. whether an enclosing
block closes on that same line, ahead of the terminator. Only when that's
true does `check_heredoc_scope_errors`
(`src/compiler/helpers_block_inline.rs`) still run its scope check; an
ordinary heredoc statement is exempted entirely, since every enclosing
block is still open at that point.

## Effect

- `CSV-AutoClass` now loads and its whole suite passes on mutsu, unmodified
  (was 0/2, blocked entirely at `use`).
- `CSV::Table` gets past this bug too, though it now hits a separate,
  unrelated blocker (a parse error in a transitive dependency's own
  transitive dependency, `Font::AFM`) — a new, distinct finding, not part
  of this fix.
- `roast/S02-literals/heredocs.t` (all 42 subtests, including the genuine
  "block 2a"/"block 4" error cases) still passes — verified against the
  release build.
- New pin: `t/heredoc-scope-in-sub-body.t` (8 subtests) covers both the
  fixed false positives (plain/hyphenated-name/unindented heredocs in a sub,
  an if-branch heredoc) and the preserved genuine errors (same-line block
  close, both `sub` and `if` forms), plus the "declared outside the closing
  block" working case and a heredoc with no variable reference at all.
