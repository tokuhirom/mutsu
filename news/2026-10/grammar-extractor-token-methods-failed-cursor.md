# Grammar tokens in `.^methods`, failed cursors from wrapped tokens, `Capture.list`

Grammar::Extractor builds a parse tree by wrapping every regex method of a
grammar: `$grammar.^methods.grep({ .WHAT ~~ Regex })`, then `&rule.wrap(...)`
with a wrapper that calls `callsame` and inspects the result. Three things
were missing:

- `.^methods` of a grammar did not list its `token`/`rule`/`regex`
  declarations (only `.^lookup`/`.^find_method` knew them). They are now
  listed as Regex method objects, `:local` and inherited alike.
- Inside such a wrapper, `callsame` on a rule that does not match returned
  `Nil`. Rakudo's regex methods return a failed cursor of the grammar
  (`.pos` is -3, `.orig`/`.from` describe the attempt), and the library reads
  `.orig` from it to report the unparsed rest. The wrapper now receives that
  cursor; the engine and `.parse` still see a failed match as no match.
- `Capture.list` is its positional part, so `c.head` inside `sub (|c)` is the
  first argument (it was the whole Capture), and `.tail`, `.join` and `for`
  over a Capture see its positionals too.

Grammar::Extractor's test file now runs its first 75 tests as rakudo does.
