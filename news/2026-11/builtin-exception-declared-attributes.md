# Built-in exception classes declare rakudo's attributes

mutsu registered its built-in exception classes with no attributes. As a
result, `X::NYI.^attributes` was empty, `X::OutOfRange.new.what` had no reader,
and `.raku` needed a rendering-only side table to list the attributes (#18654).

The classes now declare the attributes rakudo's do, as `ClassDef::attributes`.
The data comes from `src/runtime/builtin_exception_attrs.tsv`, which
`scripts/builtin-exception-attrs.raku` reads off rakudo. The table now also
records each literal initializer, so construction behaves as rakudo's does:

- `X::OutOfRange.new.what` is `Argument`.
- `X::AdHoc.new.payload` is `Unexplained error`.
- `X::Undeclared.new.is-compile-time` is `False`.
- An unset slot holds its type object: `Any`, `Mu`, `Bool`, or `Nil`.
- `X::TypeCheck`'s private `is built` `got`/`expected` keep their readers.
  Rakudo gives them explicit `method got`/`method expected`.

The built-in message formatters (`value::exception_message`) now read an
undefined attribute as absent, as rakudo's check `.defined`. So a seeded
`comment => Any` does not grow `X::OutOfRange.message` a `; (Any)` suffix.
The rendering-only `.raku` path is gone, because the ordinary attribute
rendering now sees the declarations.
