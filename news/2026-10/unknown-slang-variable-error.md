# `$~NAME` rejects unknown slang names

`$~NAME` used to compile to a constant `Str` for any name, so `$~P5Regex` or `$~Foo` looked
defined. The compiler now accepts only the built-in slangs (`MAIN`, `Quote`, `Regex`) and emits
`No grammar is known for slang 'NAME'` for anything else, matching Rakudo. Slang names registered
through `$*LANG.define_slang` are not tracked (that call ignores its name argument today); the
spot carries a `TODO` for it. Closes #10455.
