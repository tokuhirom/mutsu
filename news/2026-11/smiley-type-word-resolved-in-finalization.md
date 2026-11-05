# Native type words with a smiley are resolved in finalization

A native type word with a definiteness smiley (`int:D`, `array:U`, `num:_`) is resolved by the
classifier in `finalize_unit` (`bareword::smiley_type_word_expr`) instead of being converted to a
`Type::Definedness` node and lowered back to the same word (ADR-15760 slice 2).
`t/rakuast/rakuast-native-smiley-type-finalize.t` pins the semantics. Found on the way: mutsu
accepts `my int:D $n = 3` where rakudo dies `Type check failed in assignment to $n; expected int:D
but got Int (3)`.
