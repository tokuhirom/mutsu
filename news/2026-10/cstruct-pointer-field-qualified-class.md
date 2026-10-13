# A CStruct pointer field reads back as its class's qualified name

A pointer field of a CStruct (`has FT_Face $.face` in `FT_Size`) read back as
a handle tagged with the short class name. When the class was declared inside
a module (`unit module Font::FreeType::Raw`), it was registered as
`Font::FreeType::Raw::FT_Face`, so the handle tagged `FT_Face` answered none of
the class's methods: `No such method 'FT_Reference_Face' for invocant of type
'FT_Face'`. The handle now carries the class's registered name. Font::FreeType
0.5.16's `t/10metrics_garamond12_otc.t` passes, as under `raku` (#12934).
