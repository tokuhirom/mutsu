# A `return` in `CATCH` is not offered to the same routine's `CONTROL`

`method repl-eval { CATCH { default { exception = $_; return } } CONTROL { return
$sentinel } ... }` returned the CONTROL block's sentinel after an error, because
the `return` raised inside the handler was caught by the CONTROL phaser of the
very routine whose CATCH ran it. rakudo does not offer it there (a `return` in
the *body* still reaches CONTROL). The signal is now tagged with the CATCH
region that raised it and that region lets it pass. This is what made
Text::CodeProcessing's REPL sandbox answer `#OUT:{}` instead of `#OUT:Nil` for a
failing chunk: with it and the two parenthesized-term fixes all eight of its
test files now report the same `not ok` count as raku (0).
