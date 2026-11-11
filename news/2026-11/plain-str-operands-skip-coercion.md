# Plain `Str` operands of `eq` and `~` skip the operand coercion chain

Every string operator coerced both of its operands through
`coerce_stringy_operand`: a Proxy FETCH, a lazy Seq reify, a user
`^find_method`, an unhandled Failure, Nil's warning, a mixin's, a Str
subclass's or an object's own `Str`, ... — a dozen probes that all answer a
plain `Str` unchanged. `eq`/`ne`/`lt`/`gt`/`le`/`ge` then also ran the Blob
pair check, the utf8 decode and the junction autothreader.

`coerce_stringy_operand` now returns a plain `Str` up front, and two plain
`Str` operands of a string comparison go straight to the comparison leaf the
general path ends in. A user `infix:<eq>`/`infix:<~>` in scope is still
consulted first.

A loop comparing and concatenating one-character strings: 182.7M -> 120.6M
instructions (-34%); MCP::Server::Tool::Web `html-to-text` on a synthetic
page: 11.6M -> 10.6M instructions per repetition (callgrind, profiling build).
Refs #17385.
