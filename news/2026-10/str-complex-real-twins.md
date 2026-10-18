# A string that spells a `Complex` converts through late twins

`"1+2i".Int`, `"3.7+0i".Num` and the other real conversions of a string that numifies to a
`Complex` were answered by a by-name gate on the interpreter's slow path. They are now late
interpreter twins of `Str.Int`, `Str.Num`, `Cool.Rat`, `Cool.FatRat` and `Cool.Real` (ADR-11276
§9.190), and `Cool.UInt` answers the string itself; the gate is gone. Output is unchanged.
