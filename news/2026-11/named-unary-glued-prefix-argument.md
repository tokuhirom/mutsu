# `trim ~$x` takes `~$x` as its argument

A named unary builtin followed by a glued string/numeric prefix term (`trim ~$cell`,
`uc ~$x`, `ords ~$s`) was read as the bareword concatenated with `~$x`, so
`Magic::Run.new(file => trim ~$<rest>)` in Jupyter::Kernel handed `"trim /path"` to the
`run` magic and its cell stayed empty. The call gate now accepts a glued `~`/`-`/`+`
prefix argument after a named unary builtin, and `trim`, `trim-leading` and
`trim-trailing` are counted among them (#16585; Jupyter::Kernel `t/06-magic.rakutest`
now matches `raku`, 56/56).
