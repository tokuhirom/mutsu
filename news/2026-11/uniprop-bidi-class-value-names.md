# `uniprop` reads Bidi_Class values and one-letter category groups as property names

`'a'.uniprop('BN')` answered the General_Category (`Ll`); rakudo reads a bare property value name
as the property the value belongs to. A Bidi_Class value (`BN`, `ON`, `R`, `AL`, `WS`, ...) now
answers the Bidi_Class, and a one-letter General_Category group (`L`, `M`, `N`, `P`, `S`, `Z`,
`C`) answers that binary property as `1`/`0` (#16491).
