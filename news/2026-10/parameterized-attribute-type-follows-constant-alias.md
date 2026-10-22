# A parameterized attribute type follows a `constant` alias

`has CA2[uint16] $.kids` with `my constant CA2 = CArray` now means
`CArray[uint16]` in every frame that checks the attribute. The registry cannot
read a pad term by name, so the class declaration resolves the alias from the
declaring frame's own locals and passes it down as part of the declaration
(`ClassDeclModifiers::attr_type_aliases`). An unset typed attribute also seeds
its type object through the same resolution instead of evaluating the spelling
`CA2[uint16]` as a bare word, which used to leave a `Str`.

This fixes a CStruct class whose submethod binds such an attribute (#14438).
