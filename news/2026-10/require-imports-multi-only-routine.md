# `require Mod <&sym>` accepts a routine exported only as multi candidates

`require ::('Crypt::Libcrypt') <&crypt>` died with `X::Import::MissingSymbols`
when the module exports `crypt` only as `multi` candidates, because the module
has no single `Module::crypt` entry (its candidates are keyed
`Module::crypt/<sig>`). `required_symbol` now also accepts a routine that has
candidates under that prefix. Crypt::AnyPasswordHash's own tests (010-use,
030-hash) now match rakudo.
