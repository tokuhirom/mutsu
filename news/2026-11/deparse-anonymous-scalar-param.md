# `.DEPARSE` prints an anonymous `$` among several parameters as `$`

A pointy block with several parameters, one of them an anonymous `$`
(`-> $, $v { }`), showed the parser's internal name `$__ANON_STATE#` in its
`.AST` and so in `.DEPARSE`. The parameter's target is now the bare `$`, as in
raku.
