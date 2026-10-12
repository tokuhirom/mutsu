# A Match reaches Cool's search and text rows

Rakudo's `Match` is `Capture is Cool`, so `$/.contains(...)`, `.index`, `.substr`, `.split`, `.fmt`,
`.subst`, `.trans` and the other `Cool` text methods read the matched text. mutsu's built-in method
table now resolves them that way: the `Match` shape reaches `Cool`'s rows for those names, and a
grammar cursor or an instance of a `Match` subclass resolves through the same shape. `Str` and `Cool`
also gained a one-argument `comb` row for an `Int` chunk size or a `Str` needle.

With the rows in place, nine one-argument arms of the name-matching cascade (`contains`, `index`,
`rindex`, `substr`, `split`, `fmt`, `comb`, `sprintf`, `starts-with`/`ends-with`) are gone: the pure
arity cascades went from 189 to 180 arms (ADR-11276 §9.89, part of #11276).
