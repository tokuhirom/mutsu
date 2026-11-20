# `.origin` of interpolated blocks holding quoted strings

A `{ ... }` block interpolated into a string (`"x{ "a" ~ 'b' }y"`,
`"x{ $v eq "}" }y"`, `$v."{'u' ~ 'c'}"()`) lost its range, and every segment
after it lost its range too, as soon as its code held a quote. The scan for
the block's closing brace gave up at the first quote. It now uses the
quote-aware scan of regex code blocks: a quoted string is skipped whole, a
brace inside it is not counted, and an apostrophe inside a name is not taken
for a quote. The block, its statements and the rest of the string are placed
as on rakudo 2026.11 (#15780).
