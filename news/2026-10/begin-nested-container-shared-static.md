# An array or hash a nested BEGIN touched is shared across frames

`sub f { my @a; BEGIN @a.push(1); @a.push(2); say @a }; f(); f()` printed
`[1 2]` twice. Rakudo prints `[1 2]` and then `[1 2 2]`: its pad clone hands
every frame the static pad's own Array, so the BEGIN's push and every call's
push land in one object. ADR-0134 §2.1.3 specified the same. Slice 2 had
shipped a copy from the static cell instead and recorded it as a deviation.

The lifted body and the head of the scope now bind an `@`/`%` lexical to its
cell (`my @a := @cell`) with the parser's own bind bookkeeping, and the body
no longer copies the lexical back. That covers INIT and CHECK phasers lifted
from routines too. A scalar is still copied, as on rakudo (#10333).

On the way, `my Int @a := @src` with a typed source of a conforming element
type no longer fails with "Type check failed in assignment ... got Array".
The declaration's element-wise check tested the bound Array itself against
`Int`. The bind check already compares the element types.
