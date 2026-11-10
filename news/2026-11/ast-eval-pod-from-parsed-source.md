# EVAL($ast) reads a parsed tree's Pod from its source

`EVAL` of a tree that came from `.AST` built `$=pod` by writing its `Doc::Block` statements back as Pod, which
covers only plain shapes: a table, a comment or a margin left `$=pod` empty or wrong. A parsed tree's root origin
holds the source it came from, so the unit's Pod is read from that, with declarator docs placed at their
statements' byte offsets. A hand-built tree keeps the written-back path. Seven more `t/` files join the
`EVAL($ast)` ratchet (#16356).
