# A `for -> \Name` closure no longer reads Nil when a later loop declares `\Name`

A closure created in a single-param `for ... -> \Bla` body searched the frame's slots by name,
so a later multi-param loop `-> \Bla, $x` that gave `\Bla` a slot hijacked the capture and
the closure read `Nil`. The loop's sigilless param is now registered like a scalar one, so
the closure reads the loop's own binding (#14594; found by ValueClass's suite).
