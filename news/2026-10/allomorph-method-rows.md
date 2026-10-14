# An allomorph's methods are the method table's rows

`<1>`, `<1.5>`, `<1e0>` and `<1+2i>` used to be answered by a name list in the mixin
dispatch: about thirty string methods on the source string, everything else on the
number. The built-in method table now answers them along Rakudo's MRO
(`IntStr Allomorph Str Int Cool Any Mu`): `Str`'s rows and `Cool`'s text rows read the
string, the numeric type's rows and `Cool`'s numeric rows read the number, and `Any`'s
list methods read the allomorph as its one element. So `<1>.head`, `.max`, `.sort`
and `.pick` keep the `IntStr`, `<1e0>.join` is `1e0`, `<1e0>.Version` is `v1.e.0`, and
`<1>.Date` parses the string, as in Rakudo (ADR-11276 §9.101).
