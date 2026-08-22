# `|«@array` (flatten + hyper prefix combined) fails to parse

Discovered via the doc-diff harness on `raku-doc/doc/Language/traps.rakudoc` (around line 1948).

## Repro

```
my @chunks;
say Blob.new: |«@chunks;
```

- raku: `Blob:0x<>` (parses fine; `|«` here means something like "flatten, hyper-applied" as an
  argument-list prefix)
- mutsu: `===SORRY!=== Error while compiling ... Confused. expected statement: expected '.' or
  digits or generic radix literal or unicode numeric literal or declared term symbol or ...`

## Root cause guess

The parser's argument-list prefix operators presumably recognize `|` (flatten) and `«...»`
(hyper) separately, but not the combined `|«` sequence as a single valid prefix token/operator
in this position.

## Affected files (starting point)

- `src/parser/` — argument-list prefix parsing (`|`, slurpy-flatten operators)

## Suggested next step

Check `raku-doc/doc/Language/operators.rakudoc` for how `|«`/`|»` are documented (if at all) to
confirm the exact intended semantics before implementing — this may be a rarely-used corner of
the flatten-prefix family worth double-checking against `raku -e` with a few more variations
(`|@chunks`, `«@chunks`, `|«@chunks`) to scope exactly what's missing.
