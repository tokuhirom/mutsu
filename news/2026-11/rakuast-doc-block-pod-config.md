# Hand-built Pod blocks with configuration, margin, rows or `=config` fill `$=pod`

`EVAL` of a tree holding `RakuAST::Doc::Block` statements established `$=pod`
only for plain blocks (#16574). The write-back now also covers blocks with
configuration (`:a`, `:!b`, `:n(3)`, `:s('x')` as the reader produces them),
a `margin`, `Doc::LegacyRow` table rows (put back as written, with their
separator lines) and the `=config` directive, so the same `Pod::Block`,
`Pod::Block::Table` and `Pod::Config` objects result as for Pod written in
source (compared with rakudo 2026.09). An `EVAL` of a tree now has its own
`$=pod`; the caller's is put back afterwards, where the first version let each
EVAL append to it.
