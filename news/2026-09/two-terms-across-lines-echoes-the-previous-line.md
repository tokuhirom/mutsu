# The "two terms in a row across lines" echo now points at the missing semicolon

Found while working the parse-failure index (#7954, via #8301): rakudo and
mutsu already agreed on where this parse failure is *reported* (`at
FILE:LINE` names the line the second term sits on), but disagreed on what
the `------>` snippet echoes for it.

```
my $y = 1;
say $y;
42 if 23
is 50; 1
```

Both report `at FILE:4`. rakudo's snippet echoes line 3 (`------> 42 if
23<HERE>`) — the statement that is actually missing its semicolon, with the
caret at its end. mutsu echoed line 4 (`------>is 50; 1`) instead: the
innocent next line, with the caret nowhere near the fix.

The reported line/column and the echoed snippet used to come from the exact
same source offset, so making them differ needed a way for a `RuntimeError`
to carry two positions instead of one. `RuntimeError` gained `echo_line` /
`echo_column` (defaulting to the same as `line`/`column` for every other
parse error), and `render_parse_error` sets them specifically for this one
diagnosis — by trimming trailing whitespace off the source up to the second
term's own offset, landing on the true end of the previous line. `format_parse_error`'s
`------>` rendering now prefers the echo position when set.

`t/exceptions/two-terms-in-a-row-reports-position.t`'s assertion for this
case, which previously accepted either echo, is tightened to rakudo's exact
shape.

[#8329](https://github.com/tokuhirom/mutsu/issues/8329)
