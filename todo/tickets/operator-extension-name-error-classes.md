# A malformed operator name reports "Missing block" instead of its own class

Two `sub <category>:<…>` spellings rakudo diagnoses precisely, and mutsu answers
with the generic parse failure it falls back to when the name does not parse:

| source | rakudo | mutsu |
| --- | --- | --- |
| `sub infix:[/./] { 42 }` | `X::Syntax::Extension::TooComplex` — "Colon pair value '/./' too complex to use in name" | `Missing block` |
| `sub meow:<bar> {}` | `X::Syntax::Extension::Category` — "Cannot add tokens of category 'meow'" | `Missing block` |

Both are the same shape as the parse-diagnosis work already landed this month
(`news/2026-08/parse-error-keeps-its-exception-class.md`,
`news/2026-08/three-parse-failures-keep-their-malformed-class.md`): the parser
needs to *recognise* the construct far enough to say what is wrong with it,
rather than backtracking out of the sub-name rule and reporting the missing
block that the failed name left behind.

- **TooComplex** fires when the colon-pair value of an operator name is not a
  literal string / identifier list — a regex, a computed expression, anything
  the name cannot be spelled from. `sub infix:["@"]` and `sub infix:[sym]`
  (a constant) are both legal and already work, so the check is on the *kind* of
  the bracketed value, not on brackets.
- **Category** fires when the category before the colon is not one of the known
  operator categories (`infix`, `prefix`, `postfix`, `circumfix`,
  `postcircumfix`, `term`, `trait_mod`, `statement_control`, …). Note
  `sub meow:foo<bar> {42}` is *legal* — an extended sub name, not an operator —
  and mutsu already handles it, so the check must only fire for the
  `category:<sym>` / `category:[sym]` operator-declaration shape.

Both are needed by `roast/S06-operator-overloading/sub.t` (its assertions 21 and
28-29); the file has a third, deeper blocker recorded in
`todo/deep/block-local-operator-leaks-into-later-parses.md`.
