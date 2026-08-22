# `$0` read inside a regex-embedded code block returns the raw captured string, not a `Match` object

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Match.rakudoc:40`).

## Repro

```raku
'123' ~~ / (\d) { say $0; say $/; } \d+ /; # OUTPUT: «｢1｣␤｢1｣␤ 0 => ｢1｣␤»
```

- raku:
  ```
  ｢1｣
  ｢1｣
   0 => ｢1｣
  ```
- mutsu (`target/debug/mutsu`):
  ```
  1
  ｢1｣
   0 => ｢1｣
  ```

Only the first line differs: `say $0` inside the embedded code block prints the bare string `1`
in mutsu instead of the quoted Match gist `｢1｣` that `say $/` (line 2) and the positional-capture
list (line 3) already correctly render.

## Analysis

Outside a regex-embedded block, `$0`/`$/[0]` already resolve to proper `Match` objects (as line 2
and 3 confirm — `$/` itself gists the capture correctly). But when `$0` is read from *inside* the
embedded `{ ... }` code block mid-match, mutsu appears to bind it to the raw captured substring
(a `Str`) rather than the `Match` object that `$/`'s corresponding element holds. This looks like
a separate binding path for numbered-capture variables inside an embedded block that skips
wrapping the value as a `Match`.

## Affected files (starting point)

- `src/runtime/regex.rs` / `src/runtime/regex_parse.rs` — wherever `$0`/`$1`/... get bound into
  the embedded code block's local environment during an in-progress match, likely a different
  code path than the one that builds `$/`'s final capture list after the whole match completes.

## Suggested next step

Compare the embedded-block variable-binding code for `$0` against the post-match `$/`
capture-list construction to find where the `Match`-wrapping step is skipped for the mid-match
case.
