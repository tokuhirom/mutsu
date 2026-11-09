# `die` builds its message from several arguments and from a List argument

`die "a", "b"` (also after `try`, as an expression) concatenates the arguments' `.Str`
(`ab`), while a single List argument (`die <x y>`, `die @a`, `die (1, 2)`) is one value
whose message is its `.Str` (`x y`, `p q`, `1 2`), matching `raku`. The `die(...)` call
form keeps its comma-separated arguments apart, and the `Die` opcode records whether its
payload is a single argument.
