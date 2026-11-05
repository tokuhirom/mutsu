# A BOOT scalar box is hllized when it is bound to a Raku parameter

`nqp::box_s("x", nqp::bootstr())`, `box_i` and `box_n` boxes passed to a Raku
routine or block are now the `Str` / `Int` / `Num` they hold, as in Rakudo's
binder, and the `BOOTInt` / `BOOTNum` / `BOOTStr` type objects bind as `Int` /
`Num` / `Str`. A scalar-parameter call also hllizes a raw `VMArray` / `VMHash`
argument now, as the pointy-block path already did. Assignment, returning a
value and operators on a box still leave it raw; that remainder is on #12881.
