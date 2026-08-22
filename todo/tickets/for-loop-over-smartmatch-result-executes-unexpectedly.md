# `for EXPR ~~ /regex/ { BLOCK }` executes `BLOCK` where raku produces no output

Discovered via the doc-diff harness on `raku-doc/doc/Language/traps.rakudoc` (around line 1067).

## Repro

```
if  'x' ~~ /./ { say 'yes' }
for 'x' ~~ /./ { say 'yes' }
```

- raku: `yes` (from the `if` only — the `for` line produces no output at all)
- mutsu: `yes` / `yes` (both the `if` and the `for` print)

## Notes / possible explanation

This is filed as a doc-diff finding without a confirmed root-cause mechanism — worth
investigating from scratch. One hypothesis: `for` binds its trailing `{ BLOCK }` differently than
`if` when the loop-source expression itself ends in a regex match (`~~ /./`) — e.g. if raku's
grammar parses the block as part of the smart-match's RHS rather than as the `for`-loop's body in
this specific construct, the `for` statement would have no body to execute, producing the
observed "no output." Needs a `--target=ast`/AST-shape comparison between the `if` and `for`
forms in real raku to confirm before assuming the same about mutsu's parser.

## Affected files (starting point)

- `src/parser/` — `for`-loop body/source-expression parsing when the source ends in a
  regex-match expression
- Compare against `raku --target=ast` for both the `if` and `for` variants

## Suggested next step

Get raku's AST for `for 'x' ~~ /./ { say 'yes' }` (`raku --target=ast -e '...'`) to see whether
the block truly isn't attached as the for-loop's body, then compare with
`timeout 30 target/debug/mutsu --dump-ast` for the same snippet.
