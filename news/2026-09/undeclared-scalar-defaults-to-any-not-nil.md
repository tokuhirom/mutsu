# Undeclared scalars now default to Any, not Nil

`GetGlobal`'s not-found fallback returned `Value::NIL` for every variable read that
resolved through none of the real variable stores, including a bare name under
`no strict` (which raku auto-declares as a package variable) and a package-qualified
name like `$Foo::bar` (which auto-vivifies regardless of `strict`/`no strict`, since
`use strict` never governs `::`-qualified names). Both now read as the `Any` type
object instead, matching raku:

```
$ mutsu -e 'no strict; say $zz.WHAT; say [:$foo].raku; say $Logger::get.raku;'
(Any)
[:foo(Any)]
Any
```

`Nil` swallows method calls, so the old default masked real bugs downstream (a
missing method on the undeclared variable silently no-op'd instead of throwing
`X::Method::NotFound`). The documented `Nil` defaults for `$/`, `$!`, and the digit
capture variables (`$0`, `$1`, ...) are unaffected — those still read as `Nil`
per `S02-types/nil.t`.

Fixes #9775.
