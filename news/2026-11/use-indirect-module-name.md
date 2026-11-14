# `use ::('Name')` and parse-time META6 `provides` lookup

`use ::('⊙')` (an indirect module name given as a constant string) now parses
as the module named by the string, and the parse-time export scan resolves a
`use lib <dist root>` name through `META6.json` `provides` the way the runtime
already did. The Wheel distribution's `t/02-alias.rakutest` passes as a result.
`t/03-lexical.rakutest` and `t/07-power.rakutest` still need lexically scoped
operator overrides, tracked separately.
