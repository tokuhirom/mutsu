# Deeply nested source now fails without aborting the process

The parser checks native stack headroom while descending through terms and
statement lists. On a thread without an armed stack floor, it enforces a
nesting bound and returns a catchable `X::AdHoc` error. The LSP analysis thread
now arms the same stack guard as the CLI, so deeply nested documents use their
256 MiB stack budget instead of running unguarded.

`try EVAL` can contain a parse that reaches the nesting limit, and the LSP can
continue analysing later documents.
