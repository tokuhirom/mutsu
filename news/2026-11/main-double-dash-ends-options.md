# `--` ends MAIN's options under `:named-anywhere`

With `%*SUB-MAIN-OPTS<named-anywhere>` set, MAIN kept reading option-looking
words after a `--`: `red migration apply --test -- raku -e 'exit 0'` took the
child command's `-e` as an option of its own and printed the usage instead of
dispatching (#18479). The `--` only switched the parser to "positionals have
started", which `:named-anywhere` deliberately ignores.

`--` now ends option parsing outright, as in rakudo: every word after it is
positional, a later `--` included. The first `--` is dropped wherever it
stands, also after an ordinary positional without `:named-anywhere`
(`MAIN a -- --x=1` passes `a` and `--x=1`, not a stray `--`). Red 0.2.16's
`t/140-migration-workflow` ("CLI apply --test accepts a child command") runs
this path.
