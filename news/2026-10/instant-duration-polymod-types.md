# Instant.polymod and Duration.polymod keep Rakudo's element types

`Instant.polymod` and `Duration.polymod` answered plain `Int` elements. They now
match Rakudo: a zero element is an `Int`, any other is a `Num`, and a `Duration`
invocant's first remainder stays a `Duration`.
