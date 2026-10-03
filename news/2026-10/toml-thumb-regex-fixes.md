# Five regex fixes take TOML::Thumb to green

TOML::Thumb's grammar exposed five unrelated gaps in mutsu's regex engine.
Together with the earlier `is rw` fixes (#10361, #11077), they take the
distribution from `red`, at 3 of 176 `valid.t` assertions, to all of its
baseline assertions passing:

- **A negated property followed by a `+` union.** `<-:Cc +[\t]>` means
  every character except controls, plus tab. mutsu read `Cc +[\t]` as one
  unknown property name, so the class matched everything, and a `#` comment
  swallowed the rest of the file. A combined class whose first part is
  negative now unions its later `+` parts with that complement, as the
  bracket form `<-[a] + [b]>` already did.
- **Sigspace after a `~` goal construct.** In `rule { '[' ~ ']' <key> <kv>* }`,
  the whitespace written after the inner atom was dropped together with the
  construct, so a newline between `]` and the following `<kv>` no longer
  matched.
- **The `~` construct's inner whitespace now uses the grammar's own `ws`.**
  Before, it used the built-in one, so a grammar where comments count as
  whitespace could not put a comment after `[`.
- **A `%` separator on a quantified token preceded by other tokens.** For
  `'0b' [ <[0..1]>+ ]+ % _`, the string-based LTM expansion repeated the
  whole run `'0b' [...]` and lost the separator. It now defers to the
  per-token parser, which attaches the separator natively.
- **`｢...｣` in a regex is raw.** `｢\\｣` matches two backslashes, as in
  Rakudo, instead of one.
- **`$/` inside an embedded `{ ... }` honours `<(` and `)>`.** For example,
  `"'''" <( ... )> "'''" { make ~$/ }` now makes the text without the
  delimiters.
