# The `native` declarator is a field of `Stmt::ClassDecl`

A declaration written with the `native` declarator (`native long is Int is ctype<long> is repr<P6int> { }`)
used to carry a `__mutsu_native_decl` entry in its `custom_traits`, which the parser pushed, `convert`
filtered out again, `lower` pushed again and the registrar scanned for. `Stmt::ClassDecl` now has `is_native`,
`CompiledClassDeclPlan` carries it to `apply_native_type_traits`, and the marker trait is deleted.
