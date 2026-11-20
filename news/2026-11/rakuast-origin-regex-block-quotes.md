# `.origin` of regex code blocks holding strings, and of `Regex::Block`

A code block inside a regex (`<?{ $x("a") }>`, `<!{ ... }>`, `** {"x".chars}`)
lost every range as soon as its code held a quoted string: the scan for the
block's closing brace gave up at the first quote. It now skips a quoted string
whole, so a brace inside one (`"}"`) is not counted, while an apostrophe
inside a name (`don't-x`) is not taken for a quote. A bare code block in a
regex (`token w { a { say 1 } }`, a `Regex::Block`) is now placed like the
predicate blocks. Both match rakudo 2026.11 (#15780).
