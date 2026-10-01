# `use attributes` is lexical again

`use attributes :D/:U/:_` inside a block stayed in effect after the block closed, so
`{ use attributes :D; } class A { has Int $.x }` died as if the pragma still applied. The parse-time
smiley was a single thread-local; it now lives on the parser's `LexicalScope`, which nested blocks
inherit and which is dropped with the block. Closes #10479.
