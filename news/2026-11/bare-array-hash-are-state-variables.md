# A bare `@` / `%` is a `state` variable of its block

`(@).push($x)` and `my $v = %; $v{$x} = $x` built a fresh container on every call; in rakudo an
anonymous `@` / `%` is a `state` variable of the enclosing block, like a bare `$`, so it keeps
its contents across calls. The parser now mints a name per occurrence of a bare `@` and `%`
(`__ANON_HASH_<id>__`, as `__ANON_ARRAY_<id>__` already was) and declares it as an implicit
`state` array / hash at the top of the block, and the RakuAST lowering does the same for
`VarDeclaration::Anonymous` (#16524).
