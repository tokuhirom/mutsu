# `$obj[*-1]:delete` reaches DELETE-POS on a user class

`$po[*-10]:delete` on POFile's `POFile` object (a class that implements
`DELETE-POS` and `DELETE-KEY` itself) called `DELETE-KEY` with an empty key, so
POFile threw its `IncorrectKey` where its test expects `IncorrectIndex`.

The `:delete` opcode does not record which bracket was used, so for a user
object it picks the protocol method by the index's type: an Int goes to
`DELETE-POS`, anything else to `DELETE-KEY`. A WhateverCode subscript is
always positional, so it is now resolved against the object's `.elems` first,
as the read path already does, and then dispatched as the Int it produces:
`$po[*-10]:delete` on a two-element object calls `DELETE-POS(-8)`, as in
rakudo.

All four of POFile's test files now pass under mutsu.
