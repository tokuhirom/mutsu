# A pointy block's sigilless parameter binds an element's container

`my $b = -> \t { t = 5 }; $b(@a[0])` died with "Cannot modify an immutable Int"
because a single sigilless pointy parameter was compiled as a defs-less
`Expr::Lambda`, so the argument gate (`IndexArgRef`) never saw a container-binding
`ParamDef`. The parser now routes it through `AnonSubParams` like `@`/`%`/`&`
parameters, so it binds the element's container the way `sub (\t)` does (#14210).
