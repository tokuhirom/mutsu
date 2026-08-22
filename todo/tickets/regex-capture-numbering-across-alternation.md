# Capture group numbering across an alternation branch is wrong

Discovered via the doc-diff harness on `raku-doc/doc/Language/regexes.rakudoc` (around line
1543).

## Repro

```
if 'abcd' ~~ / a [ b (.) || (x) (y) ] (.) / {
    #                 $0     $0  $1    $2
    say ~$2;
}
```

- raku: `d` (the trailing `(.)` after the alternation group is capture number 2, since the first
  alternation branch `b (.)` contributes one capture (`$0`) and the second branch `(x) (y)`
  contributes two (`$0`, `$1`), so the numbering continues from the alternation's *maximum*
  branch-capture-count)
- mutsu: empty (`$2` doesn't hold `d`)

## Root cause guess

mutsu's capture-numbering pass likely numbers captures per-branch independently (or doesn't
correctly account for an alternation where different branches declare different numbers of
capture groups) instead of reserving numbers based on the alternation's capture-count so that
captures *after* the alternation continue numbering correctly regardless of which branch
actually matched.

## Affected files (starting point)

- `src/runtime/regex_parse.rs` — capture-group numbering/allocation for alternation (`||`, `|`)
  constructs
