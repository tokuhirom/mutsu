# `use experimental` rejects tags it does not export

`use experimental :macros` used to be accepted silently, although Rakudo
2026.09 removed the `macros` feature from the `experimental` module. mutsu now
checks every tag of `use experimental` against the ones rakudo exports
(`cached`, `pack`, `rakuast`, `smallnatives`, `will-complain`) and reports any
other tag as a compile-time error, "Error while importing from 'experimental':
no such tag '<tag>'", as rakudo does (#18983).
