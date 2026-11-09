# `=begin finish` and `=for end` are compile errors

A Pod block named after a directive (`begin`, `end`, `for`, `config`, `alias`, `finish`) after
`=begin` or `=for` is `X::Syntax::Pod::BeginWithDirective` ("=begin may not be followed by
directive 'finish'"), as on rakudo; mutsu accepted it silently. Covered by
`t/exceptions/pod-begin-with-directive.t` (identical under `raku`).
