# Typed `@`/`%` attributes keep their `:D` smiley in the metaobject

`Attribute.type` and `Attribute.auto_viv_container` of `has Int:D %.h` now report
`Associative[Int:D]` and `Hash[Int:D]` like the constructor and rakudo do (previously the
smiley was dropped, so AttrX::Mooish built `my Int %` instead of `my Int:D %`). Fixes #14389.
