# RakuAST: regex outer adverbs, match markers, properties and quoted escapes

A re-survey after the first regex-atom slice still found 119 `t/` files that
stopped at a regex literal `.AST` could not read. This slice covers the four
largest groups, each measured on rakudo 2026.09:

- **Outer adverbs.** Rakudo keeps `rx:i/a/`'s adverbs as
  `adverbs => (ColonPair::True("i"),)` around the plain body. mutsu built the
  tree from the `:i `-prefixed pattern that execution reads. So `rx:i/x/` had
  no tree at all, and `rx:m/x/` rendered an `InternalModifier` inside the
  body. The tree is now built from the written body and records the adverbs.
  `m//` accepts the same modifiers (`i m s r` and their long names) besides
  `g`.
- **`<(` / `)>`** are `Regex::MatchFrom` / `Regex::MatchTo`.
- **Unicode properties.** `<:Lu>`, `<-:L>`, `<:!Lu>` and `<[x] + :Lu>` are
  `CharClassElement::Property`, with its `negated` and `inverted` flags. A
  property with an argument (`<:Nv(1)>`) still declines.
- **Escapes in quotes.** A quoted term holds the text it denotes. `"x\ny"` and
  `"\x20"` decode through the same escape routine as every `"..."` string.
  `'a\'b'` decodes as a `q` string. The `‘...’`, `“...”` and `｢...｣` forms
  are read too. A double-quoted term that interpolates (`$x`, `@a[0]`,
  `%h<k>`, `{...}`, `&f()`) still declines. A bare `&name` or `%` is
  literal, as in rakudo.

A `StrLiteral` now renders through `Str.raku`'s own escaping, as rakudo's
does. The old copy left control characters and `{` unescaped.

Execution keeps the runtime parser's plan for the new nodes. The round-trip
ratchet grew from 2947 to 3025 files.

The work turned up three bugs outside the slice, filed as issues:
- a `｢\｣` term is "not terminated" at run time (#11569);
- a `)>` inside a capturing group is rejected (#11570);
- `make 1` renders as `Call::Name` rather than `WithoutParentheses`
  (#11571).
