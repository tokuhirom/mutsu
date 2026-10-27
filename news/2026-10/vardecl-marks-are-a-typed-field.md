# A variable declaration's pseudo-traits are a typed field

`Stmt::VarDecl` carried five more facts as `__`-prefixed entries of `custom_traits`
(`__init_sees_self`, `__shaped_decl`, `__implicit_value_type`, `__name_colonpairs` and the
`is dynamic(ARG)` argument), and `RoleDecl` carried `__my_scoped`. `RoleDecl` now has `is_lexical`
(as `ClassDecl` always did) and the five declaration facts are one `marks: VarMarks` field. The
RakuAST converter no longer filters them out of the trait list, and about thirty string scans in the
parser, compiler, VM and runtime are field reads. See ADR-10723 section 6.125.
