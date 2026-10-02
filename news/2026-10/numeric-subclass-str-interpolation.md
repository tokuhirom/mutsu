# An `Int`/`Num` subclass's own `Str` is used by interpolation and `~`

`class MyInt is Int { method Str { "mine" } }` rendered `3` for `"{$x}"`,
`"a $x b"` and `~$x`, though `$x.Str` and `put $x` said `mine`. Interpolation
and prefix `~` ask the value for `.Stringy`, and the native layer for numeric
subclasses (`builtins::numeric_subclass`) answered `.Stringy` on the numeric
payload. Rakudo's `Numeric.Stringy` is `self.Str`, so `.Stringy` is now
answered by the instance itself, like `.Str` and `.gist` already were: a
subclass's own `Str` wins, and a plain subclass still renders its payload
(#10992).
