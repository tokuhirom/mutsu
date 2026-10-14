# `::Foo::` is the stash of Foo

The parser consumed the trailing `::` of `::Foo::` and `::EXPORT::DEFAULT::`
and left the bare name, so `(::Int::).^name` answered `Int` where Rakudo says
`Stash`, and a qualified one was read as a package-routine call. A leading
`::` no longer changes a stash term: it parses as the `PseudoStash` that
`Foo::` already produced.
