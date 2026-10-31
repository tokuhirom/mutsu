# `Attribute.auto_viv_container` of an untyped `&` attribute is `Callable`

`has &.fubar is rw is mooish(:predicate)` read as `Any` while a plain `has &.code`
read as `Callable` (the second came from the earlier seeding fix). `AttrX::Mooish`
builds the value from `Attribute.auto_viv_container`, whose scalar branch started every
untyped non-`@`/`%` attribute as `Any`; a `&`-sigil one now starts as the `Callable`
type object. AttrX::Mooish's `t/010-base.rakutest` and `t/010-base.custom.rakutest` go
from 80 to 83 ok, as `raku`.
