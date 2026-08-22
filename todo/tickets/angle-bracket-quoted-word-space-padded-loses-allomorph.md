# `< 42/10 >` (space-padded inside angle brackets) doesn't produce the `RatStr` allomorph like `<42/10>` does

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/quoting.rakudoc:368`).

## Root cause hypothesis

An angle-bracket quote-words term (`< ... >`) containing a single number-like word produces an
"allomorph" — a value that is simultaneously the numeric type and a `Str`-flavored variant
(`RatStr`, `ComplexStr`, etc.) — because `<...>` is fundamentally a *string* quoting construct
that also happens to auto-coerce number-shaped words:

```raku
say <42/10>.^name;   # Rat        (no surrounding whitespace)
say <1+42i>.^name;   # Complex
say < 42/10 >.^name; # RatStr     (surrounding whitespace still normal quote-words behavior)
say < 1+42i >.^name; # ComplexStr
```

mutsu gets the tight (`<42/10>`) and the Complex-with-space (`< 1+42i >`) cases right, but for
`< 42/10 >` (space-padded Rat) it reports plain `Rat` instead of `RatStr`. This suggests
mutsu's quote-words tokenizer special-cases a *single* padded numeric word and, for the
Rat-shaped case specifically, takes a shortcut straight to the plain `Rat` value instead of
going through the same allomorph-construction path that the Complex case (and the
no-whitespace `<42/10>` case) correctly uses.

## Minimal repro

```raku
say < 42/10 >.^name;
```

- `raku`: `RatStr`
- `mutsu` (`target/debug/mutsu`): `Rat`

## Affected files (starting point)

- Quote-words (`<...>`) tokenizing/allomorph construction — likely in `src/parser/` (angle
  bracket quoting) where number-shaped single words are turned into allomorph values; the
  `Complex`/`ComplexStr` path already works correctly and is a good reference for how the
  `Rat`/`RatStr` path should behave.
