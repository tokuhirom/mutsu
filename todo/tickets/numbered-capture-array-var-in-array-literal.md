# `@0` (numbered match-capture array variable) inside a `[...]` array literal fails to parse

## Discovered while

Verifying the heredoc-scope compiler fix
(`news/2026-08/heredoc-scope-check-false-positive-on-sub-body.md`) unblocked
`CSV::Table` on mutsu. It now gets past `use CSV::Table` and further into its
dependency chain, but a NEW, unrelated blocker surfaced: `Text::Utils` pulls
in `Font::AFM` (for PDF font-metrics text-width calculations), which fails to
parse.

## Repro

```
$ mutsu -e '"abc" ~~ / (\d+) /; my $x = [ @0 ]; say $x'
Parse error: Confused: Two terms in a row

$ raku -e '"abc" ~~ / (\d+) /; my $x = [ @0 ]; say $x'
Use of Nil in numeric context
[0]
```

Reduced from `Font::AFM.rakumod:436` (`~/.zef/store/Font-AFM-1.24.10/*/lib/Font/AFM.rakumod`):

```raku
my Array $bbox = [ @0».Int ];
```

Bisected: `@0` (the numbered match-capture array variable — array-context
view of `$0`, `$1`, ... captures from the most recent `~~` match) parses fine
on its own, and `».Int` chained onto it also parses fine on its own
(`say @0».Int;` works). The failure is specific to a bare `@0` (with or
without a chained `».Int`) appearing as an element **inside a `[...]` array
literal**:

| Expression | mutsu |
| --- | --- |
| `@0` (standalone) | parses |
| `@0».Int` (standalone) | parses |
| `[ @foo ]` (ordinary named array var) | parses |
| `[ @foo».Int ]` | parses |
| `[ @0 ]` | **fails** — "Confused: Two terms in a row" |
| `[ @0.Int ]` | **fails** |
| `[@0]` (no spaces) | **fails** |

So the trigger is specifically a NUMBERED (all-digit-name) array sigil
variable as an array-literal element — not general array-literal parsing,
not `».Int`/hyper-call parsing, not `@0` parsing in other contexts.

## Root cause

Not yet investigated. Likely candidates worth checking first:
- The `[...]` array-literal element parser may special-case a leading digit
  after `@` differently than the general variable-term parser (e.g.
  confusing `@0` with an index/subscript expression, or expecting the `@`
  sigil to be followed by an identifier-shaped token specifically inside
  array-literal-element position).
- `git grep -n "CaptureVar\|ArrayVar" src/parser/primary/` for where numbered
  array captures are tokenized, and compare against
  `src/parser/*/array_literal*.rs` (or wherever `[...]` element lists are
  parsed) for how it decides where one element ends and the next begins.

## Verification

- `mutsu -e '"abc" ~~ / (\d+) /; my $x = [ @0 ]; say $x'` should not error
  (exact printed value TBD — re-check against raku's actual `[0]` output
  above once fixed).
- Re-run `Font::AFM`'s own suite and `CSV::Table`'s suite under mutsu after
  the fix — `CSV::Table` may still hit further blockers past this one; keep
  going per `docs/batteries/csv.md`'s survey until it either passes or a
  further blocker is found.
- Add a `t/` pin for `[ @0 ]` / `[ @1 ]` style numbered-capture-array
  literals.
