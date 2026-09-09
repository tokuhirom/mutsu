# `bless` preserves native scalar subclass payloads

`Mu.new` already stored the payload for `Str` and `Int` subclasses in reserved
attributes, but `Mu.bless` assembled instances through a separate constructor
path and skipped those slots. A custom `Str` constructor using
`self.bless(value => $str)` therefore rendered `Str()` instead of `$str`.

Both construction paths now share the native-subclass payload seeding helper.
The existing string and integer payload readers consequently work for
`.Str`, `.gist`, `.raku`, interpolation, and numeric comparisons on instances
created with either spelling. The regression coverage also records Rakudo's
rule that `Int.bless(value => ...)` leaves the integer payload at zero.

Pinned by `t/str-coercion-and-dispatch.t`.
