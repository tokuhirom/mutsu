# FAILGOAL receives the goal as rakudo deparses it

`'[' ~ ']' \w+` handed `FAILGOAL` the source text `']' `; rakudo (RakuAST) hands it the deparsed
atom `"]" `: a quoted literal is double-quoted with `\` and `"` escaped, and a word-only one is
bare (`'ab'` is `ab`). A multi-character literal goal reached the method as the placeholder
`goal`; with one `~` in the pattern the source now names it. `t/grammar/grammar-dynvar-failgoal-ws.t`
followed the old spelling and now pins the new one. A bracketed group goal (`~ [')' | 'x']`) still
arrives as `goal`. Found by the raku-doc differential (`Language/regexes.rakudoc:2892`).
