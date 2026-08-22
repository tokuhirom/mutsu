# Nested Unicode "curly" double quotes (`“ ... “ ... ” ... ”`) fail to parse

Discovered via the doc-diff harness on `raku-doc/doc/Language/unicode_entry.rakudoc` (around
line 532).

## Minimal repro

```raku
say "here: "no problem" at all!";
```
(using the actual Unicode curly-quote characters `“`/`”` throughout, i.e.
`say ``“``here: ``“``no problem``”`` at all!``”``;`)

- `raku`: `here: “no problem” at all!` (the doc's own explanatory comment: "You can nest them!")
- `mutsu` (`target/debug/mutsu`):
  ```
  ===SORRY!=== Error while compiling ...
  Unable to parse expression in curly double quotes; couldn't find final '”' (corresponding starter was)
  ```

The three other Unicode quote pairs tested in the same doc example (`｢...｣` corner brackets,
`”...“`/`„...”`/`„...“` reversed-direction curly pairs) all already parse correctly in mutsu —
this is specific to the `“...”` pair being **nested inside itself**.

## Root cause hypothesis

mutsu's quote-parsing for the Unicode curly-double-quote pair `“`/`”` presumably scans forward
for the first matching closer without tracking nesting depth (i.e. treats every `“` after the
first purely as literal text and stops at the very first `”`), unlike how mutsu already handles
nesting for bracket-style quote delimiters (`(...)`, `[...]`, `{...}` used with `q(...)` etc.,
which do need depth-tracking and evidently already have it, since those work). The curly-quote
lexer path needs the same nesting-depth counter: increment on each literal `“` encountered while
scanning, decrement on each `”`, and only stop at the `”` that brings the depth back to zero.

## Affected files (starting point)

- `src/parser/` — wherever Unicode quote-pair delimiters (`“”`, `‘’`, `｢｣`, `„"`, etc.) are
  recognized and their contents are scanned for the matching closer.
