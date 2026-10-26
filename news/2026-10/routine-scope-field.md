# A sub's `our` / `anon` scope is a declaration field

`Stmt::SubDecl` now carries `scope: SubScope` (`My`, `Our`, `Anon`) instead of the `__our_scoped`
and `__anon_decl` marker traits that the compiler, the VM's `RegisterSub`, the registrar and the
RakuAST converter each had to find by scanning `custom_traits`. The parser sets it where it used
to push the marker, `lower` sets it from `Sub.scope`, and `convert` renders `scope => "our"` /
`"anon"` from it. `convert::sub_scope` and its marker constant are gone. See ADR-10723 section 6.119.
