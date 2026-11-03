# RakuAST: declarator docs on named declarations

A `#|` / `#=` comment documenting a named `sub`, `method`, `class`/`role`/`grammar`/`module` or
attribute is printed by `.AST` as a trailing `.declarator-docs(leading => (...), trailing => (...))`
call, as rakudo prints it (#14880). The parser already attached each comment to a declaration;
its start offset is now recorded with the doc (`DocComment::start`) and a post-pass of `Str.AST`
(`src/rakuast/named_docs.rs`) gives the declaration node that starts there the same hidden
fields an anonymous sub carries.
