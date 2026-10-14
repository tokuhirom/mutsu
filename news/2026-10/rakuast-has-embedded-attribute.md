# RakuAST: NativeCall `HAS` attributes keep their by-value layout

A NativeCall `HAS Point $.from` attribute (stored by value inside a
`repr('CStruct')` class) now converts to rakudo 2026.09's
`VarDeclaration::Simple(scope => "HAS")` and lowers back to an embedded
attribute. The round trip used to turn it into a plain `has`, so the struct
held a pointer instead of the member: `nativesizeof` was 24 where C says 20.
Four NativeCall struct tests now pass under `MUTSU_RAKUAST=1`.

The frontend ratchet also lists the 71 other files the latest
`scripts/rakuast-frontend.sh causes` survey found passing.
