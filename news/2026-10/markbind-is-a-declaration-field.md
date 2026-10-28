# A `:=` bind group is a mark on its declaration

`Stmt::MarkBind`, the statement the parser and the compiler put in front of a bound declaration, is
gone: the declaration carries `VarMarks::bound`, and the `VarDecl` arm of the compiler reads it. The
compiler's one-shot `bind_vardecl` and `sigilless_bind_vardecl` fields, which an enclosing
`SyntheticBlock` set before compiling the declaration (and which the RHS had to be kept from
inheriting), are deleted with it: the sigilless bind is `bound && sigilless_decl`.
