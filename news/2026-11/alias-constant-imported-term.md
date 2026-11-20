# A constant aliasing an imported type is a type again

`constant CA2 = CArray[uint8]` after `use NativeCall` stopped being recognised as
a type alias (a later `CA2[uint16]` read back as a postfix index instead of a
`Type::Parameterized`) once an imported word became the typed node
`ParsedExpr::ImportedTerm` instead of a marked bareword. The constant
classifier now reads the word of an imported term as well (#18297).
