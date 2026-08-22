# `(**²)` — HyperWhatever followed directly by a postfix power operator — fails to parse

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/HyperWhatever.rakudoc:41`).

## Repro

```raku
say (**²)(1, 2, 3, 4, 5);   # OUTPUT: «(1 4 9 16 25)␤»
```

- raku: `(1 4 9 16 25)`
- mutsu: parse error —
  ```
  ===SORRY!=== Error while compiling
  Confused. expected statement: expected '.' or digits or generic radix literal or unicode
  numeric literal or declared term symbol or ...
  at ...:1
  ------>say (**²)(1, 2, 3, 4, 5);
  ```

## Isolating the root cause

The single-`Whatever` equivalent already works:

```raku
say (*²)(3);   # mutsu: 9 -- correct
```

And `**` (HyperWhatever) parses fine as a bare term in most positions:

```raku
my $h = **;      say $h.^name;   # mutsu: HyperWhatever -- OK
my $h = (**);    say $h.^name;   # mutsu: HyperWhatever -- OK
say (**).^name;                  # mutsu: HyperWhatever -- OK
```

But `**` immediately followed by the postfix superscript-power operator fails even without the
outer call-parens:

```raku
my $h = **²; say $h.^name;
# mutsu: Runtime error: X::Syntax::Malformed: Malformed initializer
```

So the bug is narrowly: the term parser recognizes a postfix `²`/`³`/etc. superscript operator
immediately following `*` (single Whatever), but not immediately following `**`
(HyperWhatever) — likely because `**` is tokenized/consumed by the exponentiation-operator
lexing path in that position (expecting a right-hand operand) rather than being recognized as
the two-character HyperWhatever term first, the same way single `*` is checked for a trailing
postfix power glyph before being treated as an infix/prefix operator elsewhere.

## Affected files (starting point)

- Term/prefix parsing for `*` (Whatever) and `**` (HyperWhatever) — locate where a trailing
  Unicode superscript digit (`²`, `³`, `⁴`, ...) is recognized as a postfix power operator after
  `*`, and extend the same check to run after consuming `**` as a HyperWhatever term.
