# An allomorph's own methods are rows of `Allomorph`

`Allomorph` declares `ACCEPTS`, `WHICH` and `raku` about the allomorph itself:
`<1>.ACCEPTS("1")` compares the string, `<1>.ACCEPTS(1)` the number,
`<1>.WHICH` is `IntStr|Int|1|Str|1` and `<1>.raku` is `IntStr.new(1, "1")`.
mutsu answered them from arms of the native method cascades (the 1-argument
`ACCEPTS` arm and two branches of the 0-argument mixin block).

They are now rows of `Allomorph` (ADR-11276 slice 5): the allomorph receiver
adapter hands these three names to them with the allomorph as the invocant,
and the cascade arms are gone. An object argument to `ACCEPTS` still reaches
the interpreter row that calls its `.Str` and `.Numeric`. `gist` is no longer
special-cased either: it is `Str`'s row on the string part (`<1e3>.gist` is
`1e3`).
