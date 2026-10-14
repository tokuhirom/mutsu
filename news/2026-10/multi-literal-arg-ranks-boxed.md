# A literal multi argument ranks as a boxed value, not a native one

`multi lit(int $x)` next to `multi lit(Int $x)` called as `lit(5)` picks the `Int`
candidate in current Rakudo (2026.09); mutsu gave the literal a synthetic native type
while ranking and picked `int`. The ranking walk no longer does; a variable declared
`int`/`num`/`str` still ranks as native (#13065).
