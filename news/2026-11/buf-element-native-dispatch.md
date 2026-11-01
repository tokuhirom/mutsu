# A `Buf` element argument dispatches as a native `uint*`

`m($buf[0])` for `multi m(byte $a)` / `multi m(Int $a)` picks the `byte` candidate
in rakudo (a `Buf` element is a native container; a `Blob` or `Array` element is a
boxed `Int`). When a named routine has a native-typed parameter at the argument's
position, a `Buf` subscript argument is now boxed in a native-typed cell, which
the multi dispatcher already reads (ADR-15309). Net::BGP `t/02-as-list.t` gets
past `nuint16($!raw[2+$i*2], $!raw[3+$i*2])` (#15856).
