# A `DECLARE` HOW is available while the class body runs

Classes declared through `EXPORTHOW::DECLARE` now install their custom HOW
before registering the class body. This matters to attribute traits that call
methods on the class meta-object while the body is still being compiled, such
as Red's `is relationship{ .foreign-key }`.

The HOW protocol remains deferred until after body registration, so methods,
attributes, and user `compose` hooks still see the completed declaration. The
parser also keeps a bare block or hash after an unknown `has` trait as the
trait argument, allowing callable and named-argument forms such as
`is relationship{ ... }` and `is column{ :unique }`.

Pinned by `t/modules/import-export/exporthow-declare-body-trait.t` and its
`t/lib/DeclareEarlyHow.rakumod` fixture. This advances the Red parse/load
slice tracked by [#7988](https://github.com/tokuhirom/mutsu/issues/7988); Red's
later meta-object attribute lookup remains a separate runtime gap.
